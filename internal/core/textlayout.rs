// Copyright © SixtyFPS GmbH <info@slint-ui.com>
// SPDX-License-Identifier: GPL-3.0-only OR LicenseRef-Slint-commercial

//! module for basic text layout
//!
//! The basic algorithm for breaking text into multiple lines:
//! 1. First we determine the boundaries for text shaping. As shaping happens based on a single font and we know that different fonts cater different
//!    writing systems, we split up the text into chunks that maximize our chances of finding a font that covers all glyphs in the chunk. This way for
//!    example arabic text can be covered by a font that has excellent arabic coverage while latin text is rendered using a different font.
//!    Shaping boundaries are always also grapheme boundaries.
//! 2. Then we shape the text at shaping boundaries, to determine the metrics of glyphs and glyph clusters (grapheme boundaries with the shapable)
//! 3. Allocate graphemes into new text lines until all graphemes are consumed:
//! 4. Loop over all graphemes:
//!     Compute the width of the grapheme
//!     Determine if the grapheme is produced by a white space character
//!     If grapheme is not at break opportunity:
//!         Add grapheme to fragment
//!         If width of current line + fragment > available width:
//!             Emit current line
//!             Current line starts with fragment
//!             Clear fragment
//!         Else:
//!              Continue
//!     Else if break opportunity at grapheme boundary is optional OR if current is space and next is optional:
//!         If width of current line + fragment <= available width:
//!              Add fragment to current line
//!              Clear fragment
//!         Else:
//!              Emit current line
//!              Current line starts with fragment
//!              Clear fragment
//!         Add grapheme to fragment
//!             
//!     Else if break opportunity at grapheme boundary is mandatory:
//!         Add fragment to current line
//!         Emit current line
//!         Clear fragment
//!         Add grapheme to fragment
//!

#[derive(Clone, Debug, Default)]
pub struct ShapedGlyph {
    pub offset_x: f32,
    pub offset_y: f32,
    pub bearing_x: f32,
    pub bearing_y: f32,
    pub width: f32,
    pub height: f32,
    pub advance_x: f32,
    pub glyph_id: Option<std::num::NonZeroU16>,
    pub glyph_cluster_index: u32,
}

pub trait TextShaper {
    fn shape_text<GlyphStorage: std::iter::Extend<ShapedGlyph>>(
        &self,
        text: &str,
        glyphs: &mut GlyphStorage,
    );
}

#[derive(PartialEq, Eq, Debug)]
pub struct ShapableText<'a> {
    text: &'a str,
    start: usize, // byte offset
    len: usize,   // byte length
}

impl<'a> ShapableText<'a> {
    fn as_str(&self) -> &'a str {
        &self.text[self.start..self.start + self.len]
    }
}

pub struct ShapeBoundaries<'a> {
    text: &'a str,
    // TODO: We should do a better analysis to find boundaries for text shaping; including
    // boundaries when the bidi level changes, the script changes or an explicit separator like
    // paragraph/lineseparator/space is encountered.
    chars: core::str::CharIndices<'a>,
    next_boundary_start: Option<usize>,
    last_script: Option<unicode_script::Script>,
}

impl<'a> ShapeBoundaries<'a> {
    pub fn new(text: &'a str) -> Self {
        let chars = text.char_indices();
        let next_boundary_start = if text.len() > 0 { Some(0) } else { None };
        Self { text, chars, next_boundary_start, last_script: None }
    }
}

impl<'a> Iterator for ShapeBoundaries<'a> {
    type Item = ShapableText<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let start = self.next_boundary_start?;

        use unicode_script::UnicodeScript;
        let (next_offset, script) = loop {
            match self.chars.next() {
                Some((byte_offset, ch)) => {
                    let script = ch.script();
                    if script != *self.last_script.get_or_insert(script) {
                        break (Some(byte_offset), Some(script));
                    }
                }
                None => {
                    break (None, None);
                }
            }
        };

        let item = ShapableText {
            text: self.text,
            start,
            len: next_offset.unwrap_or_else(|| self.text.len()) - start,
        };

        self.last_script = script;
        self.next_boundary_start = next_offset;

        Some(item)
    }
}

#[derive(Clone, Default, Debug)]
pub struct TextLine {
    start: usize,
    len: usize,
    text_width: f32, // with as occupied by the glyphs
}

impl TextLine {
    fn line_text<'a>(&self, paragraph: &'a str) -> &'a str {
        &paragraph[self.start..self.start + self.len]
    }
}

#[derive(Clone)]
struct Grapheme {
    byte_offset: usize,
    byte_len: usize,
    width: f32,
    is_whitespace: bool,
}

impl TextLine {
    fn add_grapheme(&mut self, grapheme: &Grapheme) {
        if self.len == 0 {
            self.start = grapheme.byte_offset;
        }
        self.len += grapheme.byte_len;
        self.text_width += grapheme.width;
    }
    fn add_line(&mut self, candidate: &mut TextLine) {
        if self.len == 0 {
            self.start = candidate.start;
        }
        self.len += candidate.len;
        self.text_width += candidate.text_width;
        *candidate = Default::default();
    }
}

struct GraphemeCursor<'a, Font: TextShaper> {
    font: &'a Font,
    shape_boundaries: ShapeBoundaries<'a>,
    current_shapable: ShapableText<'a>,
    glyphs: Vec<ShapedGlyph>,
    // byte offset within the current shapable
    relative_byte_offset: usize,
    glyph_index: usize,
}

impl<'a, Font: TextShaper> GraphemeCursor<'a, Font> {
    fn new(text: &'a str, font: &'a Font) -> Self {
        let mut shape_boundaries = ShapeBoundaries::new(text);

        let current_shapable =
            shape_boundaries.next().unwrap_or(ShapableText { text, start: 0, len: 0 });

        let mut glyphs = Vec::new();
        font.shape_text(current_shapable.as_str(), &mut glyphs);

        Self {
            font,
            shape_boundaries,
            current_shapable,
            glyphs,
            relative_byte_offset: 0,
            glyph_index: 0,
        }
    }
}

impl<'a, Font: TextShaper> Iterator for GraphemeCursor<'a, Font> {
    type Item = Grapheme;

    fn next(&mut self) -> Option<Self::Item> {
        if self.relative_byte_offset >= self.current_shapable.len {
            self.current_shapable = match self.shape_boundaries.next() {
                Some(shapable) => shapable,
                None => return None,
            };
            self.relative_byte_offset = 0;
            self.glyph_index = 0;
            self.glyphs.clear();
            self.font.shape_text(self.current_shapable.as_str(), &mut self.glyphs);
        }

        let mut grapheme_width: f32 = 0.;

        let mut cluster_index;
        loop {
            let glyph = &self.glyphs[self.glyph_index];
            cluster_index = glyph.glyph_cluster_index as usize;
            if cluster_index != self.relative_byte_offset {
                break;
            }
            grapheme_width += glyph.advance_x;

            self.glyph_index += 1;

            if self.glyph_index >= self.glyphs.len() {
                cluster_index = self.current_shapable.len;
                break;
            }
        }
        let grapheme_byte_offset = self.current_shapable.start + self.relative_byte_offset;
        let grapheme_byte_len = cluster_index - self.relative_byte_offset;
        let first_char = self.current_shapable.as_str()[self.relative_byte_offset..].chars().next();
        let is_whitespace = first_char.map(|ch| ch.is_whitespace()).unwrap_or_default();
        self.relative_byte_offset = cluster_index;

        Some(Grapheme {
            byte_offset: grapheme_byte_offset,
            byte_len: grapheme_byte_len,
            width: grapheme_width,
            is_whitespace,
        })
    }
}

struct TextLineBreaker<'a, Font: TextShaper> {
    line_breaks: Box<dyn Iterator<Item = (usize, unicode_linebreak::BreakOpportunity)> + 'a>, // Would be nice to get rid of that Box...
    next_break_opportunity: Option<(usize, unicode_linebreak::BreakOpportunity)>,
    grapheme_cursor: GraphemeCursor<'a, Font>,
    available_width: f32,
    current_line: TextLine,
    fragment: TextLine,
    trailing_whitespace: TextLine,
}

impl<'a, Font: TextShaper> TextLineBreaker<'a, Font> {
    fn commit_fragment(&mut self) {
        self.current_line.add_line(&mut self.fragment);
        self.current_line.add_line(&mut self.trailing_whitespace);
    }
    fn add_grapheme(&mut self, grapheme: &Grapheme) {
        if grapheme.is_whitespace {
            if self.fragment.len > 0 {
                self.trailing_whitespace.add_grapheme(&grapheme)
            }
        } else {
            self.fragment.add_grapheme(&grapheme);
            self.trailing_whitespace = Default::default();
        }
    }
    fn fragment_fits(&self) -> bool {
        self.current_line.text_width
            + self.fragment.text_width
            + self.trailing_whitespace.text_width
            <= self.available_width
    }

    pub fn new(text: &'a str, font: &'a Font, available_width: f32) -> Self {
        let mut line_breaks = unicode_linebreak::linebreaks(text);
        let next_break_opportunity = line_breaks.next();

        let grapheme_cursor = GraphemeCursor::new(text, font);

        Self {
            line_breaks: Box::new(line_breaks),
            next_break_opportunity,
            grapheme_cursor,
            available_width,
            current_line: Default::default(),
            fragment: Default::default(),
            trailing_whitespace: Default::default(),
        }
    }
}

impl<'a, Font: TextShaper> Iterator for TextLineBreaker<'a, Font> {
    type Item = TextLine;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(grapheme) = self.grapheme_cursor.next() {
            let mut line_to_emit = None;

            match self.next_break_opportunity.as_ref() {
                Some((offset, unicode_linebreak::BreakOpportunity::Mandatory))
                    if *offset == grapheme.byte_offset
                        || (*offset == grapheme.byte_offset + grapheme.byte_len
                            && grapheme.is_whitespace) =>
                {
                    self.next_break_opportunity = self.line_breaks.next();

                    self.commit_fragment();
                    line_to_emit = Some(core::mem::take(&mut self.current_line));
                    self.add_grapheme(&grapheme);
                }
                Some((offset, unicode_linebreak::BreakOpportunity::Allowed))
                    if (*offset == grapheme.byte_offset)
                        || (*offset == grapheme.byte_offset + grapheme.byte_len
                            && grapheme.is_whitespace) =>
                {
                    self.next_break_opportunity = self.line_breaks.next();

                    if self.fragment_fits() {
                        self.commit_fragment();
                    } else {
                        line_to_emit = Some(core::mem::take(&mut self.current_line));
                        self.commit_fragment();
                    }

                    self.add_grapheme(&grapheme);
                }
                _ => {
                    self.add_grapheme(&grapheme);

                    if !self.fragment_fits() {
                        line_to_emit = Some(core::mem::take(&mut self.current_line));
                        self.commit_fragment();
                    }
                }
            };

            if line_to_emit.is_some() {
                return line_to_emit;
            }
        }
        self.commit_fragment();
        if self.current_line.len > 0 {
            return Some(core::mem::take(&mut self.current_line));
        }

        None
    }
}

#[test]
fn test_shape_boundaries_simple() {
    {
        let simple_text = "Hello World";
        let mut itemizer = ShapeBoundaries::new(simple_text);
        assert_eq!(itemizer.next().map(|s| s.as_str()), Some("Hello World"));
        assert_eq!(itemizer.next(), None);
    }
}

#[test]
fn test_shape_boundaries_empty() {
    {
        let mut itemizer = ShapeBoundaries::new("");
        assert_eq!(itemizer.next(), None);
    }
}

#[test]
fn test_shape_boundaries_script_change() {
    {
        let mut itemizer = ShapeBoundaries::new("abc🍌🐒def");
        assert_eq!(itemizer.next().map(|s| s.as_str()), Some("abc"));
        assert_eq!(itemizer.next().map(|s| s.as_str()), Some("🍌🐒"));
        assert_eq!(itemizer.next().map(|s| s.as_str()), Some("def"));
        assert_eq!(itemizer.next(), None);
    }
}

#[cfg(test)]
mod shape_tests {
    use super::*;

    impl<'a> TextShaper for rustybuzz::Face<'a> {
        fn shape_text<GlyphStorage: std::iter::Extend<ShapedGlyph>>(
            &self,
            text: &str,
            glyphs: &mut GlyphStorage,
        ) {
            let mut buffer = rustybuzz::UnicodeBuffer::new();
            buffer.push_str(text);
            let glyph_buffer = rustybuzz::shape(self, &[], buffer);

            let output_glyph_generator =
                glyph_buffer.glyph_infos().iter().zip(glyph_buffer.glyph_positions().iter()).map(
                    |(info, position)| {
                        let mut out_glyph = ShapedGlyph::default();

                        out_glyph.glyph_id = std::num::NonZeroU16::new(info.glyph_id as u16);
                        out_glyph.glyph_cluster_index = info.cluster;

                        out_glyph.offset_x = position.x_offset as _;
                        out_glyph.offset_y = position.y_offset as _;
                        out_glyph.advance_x = position.x_advance as _;

                        if let Some(bounding_box) = out_glyph
                            .glyph_id
                            .and_then(|id| self.glyph_bounding_box(ttf_parser::GlyphId(id.get())))
                        {
                            out_glyph.width = bounding_box.width() as _;
                            out_glyph.height = bounding_box.height() as _;
                            out_glyph.bearing_x = bounding_box.x_min as _;
                            out_glyph.bearing_y = bounding_box.y_min as _;
                        }

                        out_glyph
                    },
                );

            // Cannot return impl Iterator, so extend argument instead
            glyphs.extend(output_glyph_generator);
        }
    }

    #[test]
    fn test_shaping() {
        use std::num::NonZeroU16;
        use TextShaper;

        let mut fontdb = fontdb::Database::new();
        let dejavu_path: std::path::PathBuf =
            [env!("CARGO_MANIFEST_DIR"), "..", "backends", "gl", "fonts", "DejaVuSans.ttf"]
                .iter()
                .collect();
        fontdb.load_font_file(dejavu_path).expect("unable to load test dejavu font");
        let font_id = fontdb.faces()[0].id;
        fontdb.with_face_data(font_id, |data, font_index| {
            let face =
                rustybuzz::Face::from_slice(data, font_index).expect("unable to parse dejavu font");

            {
                let mut shaped_glyphs = Vec::new();
                // two glyph clusters: ā́b
                face.shape_text("a\u{0304}\u{0301}b", &mut shaped_glyphs);

                assert_eq!(shaped_glyphs.len(), 3);
                assert_eq!(shaped_glyphs[0].glyph_id, NonZeroU16::new(195));
                assert_eq!(shaped_glyphs[0].glyph_cluster_index, 0);

                assert_eq!(shaped_glyphs[1].glyph_id, NonZeroU16::new(690));
                assert_eq!(shaped_glyphs[1].glyph_cluster_index, 0);

                assert_eq!(shaped_glyphs[2].glyph_id, NonZeroU16::new(69));
                assert_eq!(shaped_glyphs[2].glyph_cluster_index, 5);
            }

            {
                let mut shaped_glyphs = Vec::new();
                // two glyph clusters: ā́b
                face.shape_text("a b", &mut shaped_glyphs);

                assert_eq!(shaped_glyphs.len(), 3);
                assert_eq!(shaped_glyphs[0].glyph_id, NonZeroU16::new(68));
                assert_eq!(shaped_glyphs[0].glyph_cluster_index, 0);

                assert_eq!(shaped_glyphs[1].glyph_cluster_index, 1);

                assert_eq!(shaped_glyphs[2].glyph_id, NonZeroU16::new(69));
                assert_eq!(shaped_glyphs[2].glyph_cluster_index, 2);
            }
        });
    }
}

#[cfg(test)]
mod linebreak_tests {
    use super::*;

    // All glyphs are 10 pixels wide, break on ascii rules
    struct FixedTestFont;

    impl TextShaper for FixedTestFont {
        fn shape_text<GlyphStorage: std::iter::Extend<ShapedGlyph>>(
            &self,
            text: &str,
            glyphs: &mut GlyphStorage,
        ) {
            for (byte_offset, _) in text.char_indices() {
                let out_glyph = ShapedGlyph {
                    offset_x: 0.,
                    offset_y: 0.,
                    bearing_x: 0.,
                    bearing_y: 0.,
                    width: 10.,
                    height: 10.,
                    advance_x: 10.,
                    glyph_id: None,
                    glyph_cluster_index: byte_offset as u32,
                };
                glyphs.extend(core::iter::once(out_glyph));
            }
        }
    }

    /*
    #[test]
    fn test_empty_line_break() {
        let font = FixedTestFont;
        let text = "";
        let lines = TextLineBreaker::new(text, &font, 50.).collect::<Vec<_>>();
        assert_eq!(lines.len(), 1);
        assert_eq!(lines[0].line_text(&text), "");
    }
    */

    #[test]
    fn test_basic_line_break() {
        let font = FixedTestFont;
        let text = "Hello World";
        let lines = TextLineBreaker::new(text, &font, 50.).collect::<Vec<_>>();
        assert_eq!(lines.len(), 2);
        assert_eq!(lines[0].line_text(&text), "Hello");
        assert_eq!(lines[1].line_text(&text), "World");
    }

    #[test]
    fn test_linebreak_trailing_space() {
        let font = FixedTestFont;
        let text = "Hello              ";
        let lines = TextLineBreaker::new(text, &font, 50.).collect::<Vec<_>>();
        assert_eq!(lines.len(), 1);
        assert_eq!(lines[0].line_text(&text), "Hello");
    }

    #[test]
    fn test_forced_break() {
        let font = FixedTestFont;
        let text = "Hello\nWorld";
        let lines = TextLineBreaker::new(text, &font, 100.).collect::<Vec<_>>();
        assert_eq!(lines.len(), 2);
        assert_eq!(lines[0].line_text(&text), "Hello");
        assert_eq!(lines[1].line_text(&text), "World");
    }

    #[test]
    fn test_forced_break_multi() {
        let font = FixedTestFont;
        let text = "Hello\n\n\nWorld";
        let lines = TextLineBreaker::new(text, &font, 100.).collect::<Vec<_>>();
        assert_eq!(lines.len(), 4);
        assert_eq!(lines[0].line_text(&text), "Hello");
        assert_eq!(lines[1].line_text(&text), "");
        assert_eq!(lines[2].line_text(&text), "");
        assert_eq!(lines[3].line_text(&text), "World");
    }

    #[test]
    fn test_nbsp_break() {
        let font = FixedTestFont;
        let text = "Hello\u{00a0}World";
        let lines = TextLineBreaker::new(text, &font, 100.).collect::<Vec<_>>();
        assert_eq!(lines.len(), 1);
        assert_eq!(lines[0].line_text(&text), "Hello\u{00a0}Wor"); // FIXME: let line overflow
    }
}
