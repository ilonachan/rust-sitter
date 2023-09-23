use std::{
    ffi::{c_char, c_uint, c_void},
    marker::PhantomData,
};

pub trait ExternalTokens : Sized {
    fn token_count() -> u16;
    fn token_list_index(&self) -> u16;
    fn from_index(index: u16) -> Option<Self>;
}

/// This trait must be implemented by an external scanner for your grammar.
/// It contains basic functionality required by the Tree-Sitter API.
///
/// Aside from the obvious [`scan`] functionality, all `ExternalScanner` types
/// must be creatable on-demand by the parser, and all their internal state
/// should be serializable to a byte array; these are hard requirements set by
/// Tree-Sitter, required to facilitate its "tree editing" functionality.
///
/// This means that you need to provide a way to instantiate a [`new`](ExternalScanner::new) scanner,
/// and to [`serialize`] it into a `Vec<u8>` as well as [`deserialize`] it from a `u8` slice.
///
/// Note that the original API description also includes a `destroy` function:
/// this functionality is managed by Rust using the [`Drop`] trait, and your
/// scanner is allowed to implement its own for further control.
pub trait ExternalScanner {
    type Tokens: ExternalTokens;

    /// Create a new scanner instance.
    fn new() -> Self;
    /// Copy the scanner's entire state into a byte buffer, and return that buffer.
    /// Note that the maximum number of bytes is limited by the constant
    /// `TREE_SITTER_SERIALIZATION_BUFFER_SIZE`, which I cannot currently obtain.
    /// TODO: fix that.
    /// TODO: encode the size requirement in the types somehow.
    ///
    /// This serialized state is stored in the syntax tree to allow easy restoring
    /// for edits and ambiguities. You should design your scanner to (de)serialize
    /// its state quickly and compactly.
    ///
    /// While it may not always be the most efficient, for simple cases we recommend
    /// Rust's very mature [`serde`] functionality along with a crate like [`bincode`].
    /// Sadly there is no good way to automatically apply this recommendation.
    fn serialize(&mut self) -> Vec<u8>;
    /// Restore the state of the scanner based on the serialized data provided.
    /// This data is always the result of a previous [`serialize`] call.
    ///
    /// While it may not always be the most efficient, for simple cases we recommend
    /// Rust's very mature [`serde`] functionality along with a crate like [`bincode`].
    /// Sadly there is no good way to automatically apply this recommendation.
    /// 
    /// WARNING: this function is once called with an empty data array. If your serialization
    /// isn't empty, i.e. your scanner is stateful, consider this an "initialization" where
    /// the default values are prepared.
    fn deserialize(&mut self, serialized: &[u8]);
    /// The actual scanner logic: recognize an external token, if possible.
    /// May return the matched token, or `None` if no token was valid or an error occured.
    ///
    /// The `lexer` is a kind of "reading head" that allows reading the character
    /// at the current position, as well as advancing that position; it also returns
    /// some other helpful information, like column number or end-of-file marking.
    ///
    /// `symbol_expected` is a safe mapping from each possible token to a bool,
    /// indicating if the token would be valid in this location according to the grammar.
    /// In error correction mode, this is true for all tokens, even those which by design
    /// can never appear in the grammar.
    ///
    /// We internally use this fact to use a dummy token that can never be matched,
    /// and if that token's expected value is set to true we know we're in error
    /// correction mode. The result of this test is provided in `is_error_correction`.
    fn scan(
        &mut self,
        lexer: &mut TSLexer<Self::Tokens>,
        symbol_expected: impl Fn(Self::Tokens) -> bool,
        is_error_correction: bool,
    ) -> Option<Self::Tokens>;
}

pub type TSSymbol = u16;
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct _TSLexer {
    pub lookahead: i32,
    pub result_symbol: TSSymbol,
    pub advance: Option<unsafe extern "C" fn(lexer: *mut _TSLexer, skip: bool)>,
    pub mark_end: Option<unsafe extern "C" fn(lexer: *mut _TSLexer)>,
    pub get_column: Option<unsafe extern "C" fn(lexer: *mut _TSLexer) -> u32>,
    pub is_at_included_range_start: Option<unsafe extern "C" fn(lexer: *const _TSLexer) -> bool>,
    pub eof: Option<unsafe extern "C" fn(lexer: *const _TSLexer) -> bool>,
}

/// A wrapper around the internal lexer object, used by the Tree-Sitter external scanner API
/// to allow scanner logic to "move the lexing head".
pub struct TSLexer<T: ExternalTokens> {
    internal: *mut _TSLexer,
    ph: PhantomData<T>,
}
impl<T: ExternalTokens> TSLexer<T> {
    /// Returns the current next character in the input stream, represented as a
    /// 32-bit unicode code point. This function has no overhead, and only
    /// updates if the lexer is [`advance`]d.
    ///
    /// TODO: potentially find a better value type: we can't use `char` because the character need not be valid (on its own)
    pub fn lookahead(&self) -> u32 {
        unsafe { (*self.internal).lookahead as u32 }
    }

    /// The scan function should assign to this field the symbol that was
    /// recognized, one of the external tokens.
    ///
    /// Can be called multiple times, only the final value is used.
    /// But for more rusty design, the user shouldn't "set the token and return
    /// true if there is one", but instead just return an `Option<Token>`;
    /// therefore this assignment is handled automatically.
    fn result_symbol(&mut self, token: T) {
        unsafe {
            (*self.internal).result_symbol = token.token_list_index();
        }
    }

    /// Advance to the next character. Passing `true` will cause the current
    /// character to be treated as whitespace, and excluded from the text range
    /// associated with tokens emitted by the external scanner.
    pub fn advance(&mut self, skip: bool) {
        unsafe { (*self.internal).advance.unwrap()(self.internal, skip) }
    }

    /// Mark the end of the recognized token. This allows matching tokens that
    /// require multiple characters of overhead:
    ///
    /// By default any character that you move past using [`advance`] will be
    /// included in the token size, but calling [`mark_end`] means later calls
    /// to `advance` will not increase the returned token size.
    ///
    /// `mark_end` can be called multiple times, and will each time update the
    /// token's max size (increasing it).
    pub fn mark_end(&mut self) {
        unsafe { (*self.internal).mark_end.unwrap()(self.internal) }
    }

    /// Queries the current column position of the lexer: returns the number of
    /// codepoints since the start of the current line.
    ///
    /// The codepoint position is recalculated every time the function is called,
    /// by reading from the start of the line.
    pub fn get_column(&mut self) -> u32 {
        unsafe { (*self.internal).get_column.unwrap()(self.internal) }
    }

    /// Check whether the parser has just skipped some characters in the document.
    ///
    /// When parsing an embedded document using the `ts_parser_set_included_ranges`
    /// (TODO: Rust equivalent?)
    /// function (described in Tree-Sitter documentation under
    /// [Multi-Language Documents](https://tree-sitter.github.io/tree-sitter/using-parsers#multi-language-documents)),
    /// the scanner may want to apply some special behavior when moving to a disjoint part of the document.
    /// For example, in [EJS Documents](https://ejs.co/) the JavaScript parser uses this function
    /// to enable inserting automatic semicolon tokens between the code directives,
    /// delimited by `<%` and `%>`.
    pub fn is_at_included_range_start(&self) -> bool {
        unsafe { (*self.internal).is_at_included_range_start.unwrap()(self.internal) }
    }

    /// Check whether the lexer is at the end of the file. Note that while [`lookahead`]
    /// will always return `0` in that case, this could be a valid character in the
    /// file, and therefore this function is the correct way to do end-of-file checking.
    pub fn eof(&self) -> bool {
        unsafe { (*self.internal).eof.unwrap()(self.internal) }
    }
}
impl<T: ExternalTokens> From<*mut _TSLexer> for TSLexer<T> {
    fn from(value: *mut _TSLexer) -> Self {
        TSLexer {
            internal: value,
            ph: PhantomData,
        }
    }
}

/// Internally this function is called by Tree-Sitter to create the desired [`ExternalScanner`].
/// It is expected to return a mutable pointer, so the only reasonable way to do this is to
/// allocate the scanner object on the heap.
///
/// This function exists to provide the logic for the C interop function
/// `tree_sitter_<your language name>_external_scanner_create`,
/// which would be automatically generated by the macros.
/// It is not intended to be called in any other scenario!
pub unsafe fn create<T: ExternalScanner>() -> *mut c_void {
    // This function isn't actually unsafe, but we still don't want it to just be
    // accidentally called from regular Rust code.

    let b = Box::new(T::new());
    Box::into_raw(b) as *mut c_void
}
/// Internally this function is called by Tree-Sitter to destroy an existing [`ExternalScanner`].
/// The pointer provided to this function should always be one of the references returned by [`create`],
/// but sadly there is no way to check this.
///
/// This function exists to provide the logic for the C interop function
/// `tree_sitter_<your language name>_external_scanner_destroy`,
/// which would be automatically generated by the macros.
/// It is not intended to be called in any other scenario!
pub unsafe fn destroy<T: ExternalScanner>(payload: *mut c_void) {
    unsafe {
        // We have to trust that `payload` actually points to a valid `ExternalScanner` instance.
        drop(Box::<T>::from_raw(payload as *mut T));
    }
}
/// Internally this function is called by Tree-Sitter to serialize an [`ExternalScanner`],
/// and store the produced byte array in the provided buffer (returning the actual length of the data).
///
/// This function exists to provide the logic for the C interop function
/// `tree_sitter_<your language name>_external_scanner_serialize`,
/// which would be automatically generated by the macros.
/// It is not intended to be called in any other scenario!
pub unsafe fn serialize<T: ExternalScanner>(payload: *mut c_void, buffer: *mut c_char) -> c_uint {
    // This intermediate step requires heap allocation and might be unfortunately slow...
    // TODO: maybe the rust function should mirror the original syntax after all?
    // technically UNSAFE, but this will never cause any problems: conversion from `u8` to `i8` is bit bashing anyway.
    let buf: Vec<i8> = unsafe { std::mem::transmute((*(payload as *mut T)).serialize()) };
    if buf.len() > c_uint::MAX.try_into().unwrap() {
        panic!("buffer was not long enough to store the state");
    }
    unsafe {
        // The check above serves to make sure that `buf` isn't too long to fit
        // inside the buffer. We HAVE TO obtain the actual buffer size from code to actually
        // make this safe! We also need to trust that the buffer is actually valid and big enough.
        std::ptr::copy_nonoverlapping(buf.as_ptr(), buffer, buf.len());
    }
    return buf.len().try_into().unwrap();
}
/// Internally this function is called by Tree-Sitter to deserialize an [`ExternalScanner`],
/// loading its information from the provided byte array.
///
/// This function exists to provide the logic for the C interop function
/// `tree_sitter_<your language name>_external_scanner_deserialize`,
/// which would be automatically generated by the macros.
/// It is not intended to be called in any other scenario!
pub unsafe fn deserialize<T: ExternalScanner>(
    payload: *mut c_void,
    buffer: *const c_char,
    length: c_uint,
) {
    let slice: &[u8] = unsafe {
        // We have to trust that C gives us a valid buffer + length.
        // The `transmute` is just to bit-bash the signedness of `c_char` away.
        std::mem::transmute(std::slice::from_raw_parts(
            buffer,
            length.try_into().unwrap(),
        ))
    };
    (*(payload as *mut T)).deserialize(slice);
}
/// Internally this function is called by Tree-Sitter to call the actual scanning logic of an [`ExternalScanner`].
///
/// The `valid_symbols` table describes which tokens can be used, but it is also our only way to detect error
/// correction mode; this requires us to put a sentinel token at the end of the token enum, and this function
/// automatically deals with filtering out that result.
///
/// The return value indicates if a token was matched, and if so the `lexer`'s [`result_symbol`](_TSLexer::result_symbol)
/// describes which one it was. This function wraps that by allowing the actual scanner function to instead return
/// an `Option` containing the specific token, if one was matched.
///
/// This function exists to provide the logic for the C interop function
/// `tree_sitter_<your language name>_external_scanner_scan`,
/// which would be automatically generated by the macros.
/// It is not intended to be called in any other scenario!
pub unsafe fn scan<T: ExternalScanner>(
    payload: *mut c_void,
    lexer: *mut _TSLexer,
    valid_symbols: *const bool,
) -> bool {
    let max_symbol_index: u16 = T::Tokens::token_count();
    let mut lex_obj = lexer.into();

    let (symbol_getter, is_error_correction) = unsafe {
        // We have to trust that `valid_symbols` actually points to a `bool` array with as many entries as
        // external tokens were declared. We also MUST ENSURE that there's at least one sentinel token auto-inserted
        // by the macro, which doesn't count towards the [`ExternalTokens`] `token_count`

        let valid_symbol_list =
            std::slice::from_raw_parts(valid_symbols, (max_symbol_index + 1).into());
        let symbol_getter = move |index: T::Tokens| {
            if index.token_list_index() >= max_symbol_index {
                panic!("invalid symbol data accessed");
            }
            // We've already checked the length of the list above
            valid_symbol_list
                .get_unchecked::<usize>(index.token_list_index().into())
                .to_owned()
        };
        // We've already assumed that the list has the required length
        let is_error_correction = valid_symbol_list
            .get_unchecked::<usize>(max_symbol_index.into())
            .to_owned();

        (symbol_getter, is_error_correction)
    };

    let result_token = (*(payload as *mut T)).scan(&mut lex_obj, symbol_getter, is_error_correction);
    match result_token {
        Some(token) => {
            lex_obj.result_symbol(token);
            return true;
        }
        None => return false,
    }
}
