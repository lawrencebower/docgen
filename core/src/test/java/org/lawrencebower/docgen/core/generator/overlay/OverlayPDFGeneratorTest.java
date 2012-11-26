package org.lawrencebower.docgen.core.generator.overlay;

import com.lowagie.text.pdf.PdfReader;
import com.lowagie.text.pdf.PdfStamper;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.generator.overlay.component.OverlayComponent;
import org.lawrencebower.docgen.core.generator.overlay.component.OverlayTextComponent;
import org.lawrencebower.docgen.core.generator.utils.PDFGenUtilsImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.io.OutputStream;
import java.util.Arrays;
import java.util.List;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.*;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = {"classpath:META-INF/core-test-context.xml"})
public class OverlayPDFGeneratorTest {

    @Autowired
    private OverlayPDFGenerator pdfGenerator;

    @Test
    public void testGeneratePDF_generatePDF_checksRequiredValuesPresent() throws Exception {

        PDFGenUtilsImpl mockUtils = mockPDFUtils();

        pdfGenerator.setPdfGenUtils(mockUtils);

        OverlayDocument document = stubDocument();

        pdfGenerator.generatePDF(document);

        verify(mockUtils).checkRequiredValuesPresent(document);

    }

    private PDFGenUtilsImpl mockPDFUtils() {

        PDFGenUtilsImpl mockPDFUtils = mock(PDFGenUtilsImpl.class);

        PdfReader mockPDFReader = mock(PdfReader.class);

        PdfStamper mockPdfStamper = mock(PdfStamper.class);

        when(mockPDFUtils.getPDFReaderAndUnlockForSourcePDF(anyString())).thenReturn(mockPDFReader);
        when(mockPDFUtils.getPDFStamper(any(PdfReader.class), any(OutputStream.class))).thenReturn(mockPdfStamper);

        return mockPDFUtils;
    }

    public OverlayDocument stubDocument() {

        OverlayDocument document = mock(OverlayDocument.class);

        when(document.getDocType()).thenReturn(null);
        List<OverlayComponent> textComponents = (Arrays.asList((OverlayComponent) mock(OverlayTextComponent.class)));
        when(document.getComponents()).thenReturn(textComponents);
        when(document.getName()).thenReturn("name");

        return document;
    }

}
