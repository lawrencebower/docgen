package org.lawrencebower.docgen.core.document.component.text;

public class TextFragment {

    private String text;

    private FontInfo fontInfo;

    public TextFragment(String text, FontInfo fontInfo) {
        this.text = text;
        this.fontInfo = fontInfo;
    }

    public FontInfo getFontInfo() {
        return fontInfo;
    }

    public String getText() {
        return text;
    }
}
