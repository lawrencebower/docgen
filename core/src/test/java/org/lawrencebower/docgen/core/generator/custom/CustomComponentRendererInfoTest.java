package org.lawrencebower.docgen.core.generator.custom;

import com.lowagie.text.Paragraph;
import org.junit.Test;
import org.lawrencebower.docgen.core.exception.DocGenException;

import java.io.ByteArrayOutputStream;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class CustomComponentRendererInfoTest {

    @Test(expected = DocGenException.class)
    public void testPreparePDFWriter_nullOutputStream_throwsError() throws Exception {
        new CustomComponentRendererInfo(null);
    }

    @Test
    public void testPreparePDFWriter_validOutputStream_noErrors() throws Exception {
        ByteArrayOutputStream outStream = getOutStream();
        new CustomComponentRendererInfo(outStream);
    }

    @Test
    public void testAddToDocument_validParagraph_noError() throws Exception {
        CustomComponentRendererInfo rendererInfo = getRendererInfo();
        rendererInfo.addToDocument(new Paragraph());
    }

    @Test
    public void testAddToDocument_validParagraph_dataWrittenTrue() throws Exception {
        CustomComponentRendererInfo rendererInfo = getRendererInfo();
        rendererInfo.addToDocument(new Paragraph());
        assertTrue(rendererInfo.documentDataWritten);
    }

    @Test
    public void testAddToDocument_notCalled_dataWrittenFalse() throws Exception {
        CustomComponentRendererInfo rendererInfo = getRendererInfo();
        assertFalse(rendererInfo.documentDataWritten);
    }

    @Test(expected = DocGenException.class)
    public void testAddToDocument_nullParagraph_error() throws Exception {
        CustomComponentRendererInfo rendererInfo = getRendererInfo();
        rendererInfo.addToDocument(null);
    }

    @Test
    public void testCloseDocument_noDataWritten_noError() throws Exception {
        CustomComponentRendererInfo rendererInfo = getRendererInfo();
        rendererInfo.closeDocument();
    }

    @Test
    public void testCloseDocument_dataWritten_noError() throws Exception {
        CustomComponentRendererInfo rendererInfo = getRendererInfo();
        Paragraph paragraph = new Paragraph("testText");
        rendererInfo.addToDocument(paragraph);
        rendererInfo.closeDocument();
    }

    @Test(expected = DocGenException.class)
    public void testCloseDocument_emptyDataWritten_error() throws Exception {
        CustomComponentRendererInfo rendererInfo = getRendererInfo();
        Paragraph emptyParagraph = new Paragraph();
        rendererInfo.addToDocument(emptyParagraph);
        rendererInfo.closeDocument();
    }

    ///////////////////////////

    private CustomComponentRendererInfo getRendererInfo() {
        ByteArrayOutputStream outStream = getOutStream();
        return new CustomComponentRendererInfo(outStream);
    }

    private ByteArrayOutputStream getOutStream() {
        return new ByteArrayOutputStream();
    }

}
