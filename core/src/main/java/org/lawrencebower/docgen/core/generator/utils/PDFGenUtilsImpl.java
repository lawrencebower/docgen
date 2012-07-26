package org.lawrencebower.docgen.core.generator.utils;

import com.lowagie.text.DocumentException;
import com.lowagie.text.Font;
import com.lowagie.text.pdf.BaseFont;
import org.lawrencebower.docgen.core.document.DocumentInfo;
import org.lawrencebower.docgen.core.exception.DocGenException;

import java.io.IOException;

public class PDFGenUtilsImpl implements PDFGenUtils {

    private float DEFAULT_LEADING = 9;

    @Override
    public void checkRequiredValuesPresent(DocumentInfo docInfo) {
        if (docInfo.getDocType() == null) {
            throw new DocGenException("DocInfo DocType must not be null");
        }

        if (docInfo.getComponents() == null || docInfo.getComponents().isEmpty()) {
            throw new DocGenException("DocInfo Document components are null/empty");
        }

        if (docInfo.getName() == null) {
            throw new DocGenException("DocInfo Name must not be null");
        }
    }

    @Override
    public Font getDefaultFont() {
        try {
            BaseFont baseFont = BaseFont.createFont();
            return new Font(baseFont, 10);
        } catch (DocumentException | IOException e) {
            throw new DocGenException(e);
        }
    }

    @Override
    public float getLeading() {
        return DEFAULT_LEADING;
    }

}
