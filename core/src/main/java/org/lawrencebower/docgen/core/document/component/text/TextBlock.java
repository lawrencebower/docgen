package org.lawrencebower.docgen.core.document.component.text;

import java.util.ArrayList;
import java.util.List;

public class TextBlock {

    List<TextFragment> fragments = new ArrayList<>();

    public TextBlock(String text) {
        addFragment(text);
    }

    public TextBlock(TextFragment... fragments) {
        for (TextFragment block : fragments) {
            addFragment(block);
        }
    }

    public TextBlock(String text, FontInfo fontInfo) {
        addFragment(text, fontInfo);
    }

    public void addFragment(String text) {
        addFragment(text, FontInfo.createDefaultInfo());
    }

    public void addFragment(String text, FontInfo fontInfo) {
        TextFragment fragment = new TextFragment(text, fontInfo);
        addFragment(fragment);
    }

    public void addFragment(TextFragment fragment) {
        fragments.add(fragment);
    }

    public void setFragments(List<TextFragment> fragments) {
        this.fragments = fragments;
    }

    public List<TextFragment> getFragments() {
        return fragments;
    }

    public void addFragments(List<TextFragment> fragments) {
        this.fragments.addAll(fragments);
    }
}
