package org.lawrencebower.docgen.core.generator.utils;

import com.lowagie.text.Font;
import org.lawrencebower.docgen.core.document.DocumentInfo;

public interface PDFGenUtils {

    void checkRequiredValuesPresent(DocumentInfo docInfo);

    Font getDefaultFont();

    float getLeading();
}
