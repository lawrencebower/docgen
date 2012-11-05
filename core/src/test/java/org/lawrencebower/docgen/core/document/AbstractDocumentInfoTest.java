package org.lawrencebower.docgen.core.document;

import org.junit.Test;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.core.generator.custom.CustomDocumentInfo;
import org.lawrencebower.docgen.core.generator.custom.CustomPDFGenerator;
import org.mockito.Mockito;

import static junit.framework.Assert.assertEquals;

public class AbstractDocumentInfoTest {

    @Test
    public void testConstructor_emptyNameSet_throwsError() {
        try {
            CustomPDFGenerator mockGenerator = Mockito.mock(CustomPDFGenerator.class);
            new CustomDocumentInfo("", mockGenerator);
        } catch (DocGenException e) {
            assertEquals("Name is not set", e.getMessage());
        }
    }

    @Test
    public void testConstructor_nullNameSet_throwsError() {
        try {
            CustomPDFGenerator mockGenerator = Mockito.mock(CustomPDFGenerator.class);
            new CustomDocumentInfo(null, mockGenerator);
        } catch (DocGenException e) {
            assertEquals("Name is not set", e.getMessage());
        }
    }
}
