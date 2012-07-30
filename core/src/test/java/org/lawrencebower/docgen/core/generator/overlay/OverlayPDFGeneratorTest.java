package org.lawrencebower.docgen.core.generator.overlay;

import com.lowagie.text.pdf.PdfReader;
import com.lowagie.text.pdf.PdfStamper;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.document.OverlayDocumentInfo;
import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.generator.overlay.renderer.OverlayComponentRenderer;
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

        stubComponentRenderer();

        OverlayDocumentInfo documentInfo = stubDocInfo();

        pdfGenerator.generatePDF(documentInfo);

        verify(mockUtils).checkRequiredValuesPresent(documentInfo);

    }

    private PDFGenUtilsImpl mockPDFUtils() {

        PDFGenUtilsImpl mockPDFUtils = mock(PDFGenUtilsImpl.class);

        PdfReader mockPDFReader = mock(PdfReader.class);

        PdfStamper mockPdfStamper = mock(PdfStamper.class);

        when(mockPDFUtils.getPDFReaderForSourcePDF(anyString())).thenReturn(mockPDFReader);
        when(mockPDFUtils.getPDFStamper(any(PdfReader.class), any(OutputStream.class))).thenReturn(mockPdfStamper);

        return mockPDFUtils;
    }

    private void stubComponentRenderer() {
        OverlayComponentRenderer componentRenderer = mock(OverlayComponentRenderer.class);
        pdfGenerator.setComponentRenderer(componentRenderer);
    }

    public OverlayDocumentInfo stubDocInfo() {

        OverlayDocumentInfo docInfo = mock(OverlayDocumentInfo.class);

        when(docInfo.getDocType()).thenReturn(null);
        List<DocComponent> textComponents = (Arrays.asList((DocComponent) mock(TextComponent.class)));
        when(docInfo.getComponents()).thenReturn(textComponents);
        when(docInfo.getName()).thenReturn("name");

        return docInfo;
    }

}
