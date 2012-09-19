package org.lawrencebower.docgen.web.Test;

import org.lawrencebower.docgen.core.generator.utils.PDFGenUtilsImpl;

public class Test {
    public static void main(String[] args) {
        System.out.println("Test.main");
        int defaultFont = PDFGenUtilsImpl.DEFAULT_FONT;
        System.out.println("defaultFont = " + defaultFont);
    }
}
