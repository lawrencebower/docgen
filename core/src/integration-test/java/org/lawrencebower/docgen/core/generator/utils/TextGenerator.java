package org.lawrencebower.docgen.core.generator.utils;

import org.lawrencebower.docgen.core.document.component.text.FontInfo;
import org.lawrencebower.docgen.core.document.component.text.FontStyle;
import org.lawrencebower.docgen.core.document.component.text.TextBlock;
import org.lawrencebower.docgen.core.document.component.text.TextFragment;

import java.util.Arrays;
import java.util.List;

public class TextGenerator {

    public static List<TextBlock> createVariedTextBlocks() {

        TextBlock block1 = new TextBlock("plain text dialog 10", new FontInfo("Dialog", 10, FontStyle.PLAIN));

        TextBlock block2 = new TextBlock("bold text monospaced 16", new FontInfo("Monospaced", 16, FontStyle.BOLD));

        TextBlock block3 = new TextBlock("bold italic text serif 20", new FontInfo("Serif", 20, FontStyle.BOLD_ITALIC));

        TextBlock block4 = new TextBlock("underline text sansserif 24", new FontInfo("SansSerif", 10, FontStyle.UNDERLINE));

        return Arrays.asList(block1, block2, block3, block4);
    }

    public static TextBlock createVariedTextBlock() {

        TextBlock singleBlock = new TextBlock();

        for (TextBlock textBlock : createVariedTextBlocks()) {
            List<TextFragment> fragments = textBlock.getFragments();
            singleBlock.addFragments(fragments);
        }

        return singleBlock;
    }

    public static String multiplyText(String text) {
        StringBuilder builder = new StringBuilder();

        for (int i = 0; i < 100; i++) {
            builder.append(text);
            builder.append(" ");
        }

        builder.append("\n\n");

        return builder.toString();
    }
}
