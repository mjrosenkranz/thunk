/// location of the token in the source file
/// this is useful for debugging and such
pub const SrcLoc = struct {
    // name of the file we are scanning
    file: []const u8 = "unknown",
    // the line number of this token
    line: u32 = 0,
    // the column number of this token
    col: u32 = 0,
    // slice of the source code that this token refers to
    slice: []const u8 = "error",
};
