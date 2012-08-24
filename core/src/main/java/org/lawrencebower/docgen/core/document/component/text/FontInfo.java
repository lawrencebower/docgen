package org.lawrencebower.docgen.core.document.component.text;

public class FontInfo {

    private int fontSize;
    private String font;
    private FontStyle style;

    public static final int DEFAULT_FONT_SIZE = 10;
    public static final String DEFAULT_FONT = "Helvetica";
    public static final FontStyle DEFAULT_FONT_STYLE = FontStyle.PLAIN;

    public FontInfo(String font,
                    int fontSize,
                    FontStyle style) {

        this.fontSize = fontSize;
        this.font = font;
        this.style = style;
    }

    public static FontInfo createDefaultInfo() {
        return new FontInfo(DEFAULT_FONT,
                            DEFAULT_FONT_SIZE,
                            DEFAULT_FONT_STYLE);
    }

    public int getFontSize() {
        return fontSize;
    }

    public String getFont() {
        return font;
    }

    public FontStyle getStyle() {
        return style;
    }

    public static FontInfo DEFAULT_BOLD() {
        return new FontInfo(DEFAULT_FONT,
                            DEFAULT_FONT_SIZE,
                            FontStyle.BOLD);
    }

    public static FontInfo DEFAULT() {
        return new FontInfo(DEFAULT_FONT,
                            DEFAULT_FONT_SIZE,
                            FontStyle.PLAIN);
    }
}
