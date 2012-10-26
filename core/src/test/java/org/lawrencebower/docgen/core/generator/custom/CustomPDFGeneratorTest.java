package org.lawrencebower.docgen.core.generator.custom;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.document.CustomDocumentInfo;
import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.generator.utils.PDFGenUtilsImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.util.Arrays;
import java.util.List;

import static org.mockito.Mockito.*;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = {"classpath:META-INF/core-test-context.xml"})
public class CustomPDFGeneratorTest {

    @Autowired
    private CustomPDFGenerator pdfGenerator;

    @Test
    public void testGeneratePDF_generatePDF_checksRequiredValuesPresent() throws Exception {

        PDFGenUtilsImpl mockUtils = mock(PDFGenUtilsImpl.class);

        pdfGenerator.setPdfGenUtils(mockUtils);

        stubComponentRenderer();

        CustomDocumentInfo documentInfo = stubDocInfo();

        pdfGenerator.generatePDF(documentInfo);

        verify(mockUtils).checkRequiredValuesPresent(documentInfo);

    }

    private void stubComponentRenderer() {
        CustomComponentRenderer componentRenderer = mock(CustomComponentRenderer.class);
        pdfGenerator.setComponentRenderer(componentRenderer);
    }

    public CustomDocumentInfo stubDocInfo() {

        CustomDocumentInfo docInfo = mock(CustomDocumentInfo.class);

        when(docInfo.getDocType()).thenReturn(null);
        List<DocComponent> textComponents = (Arrays.asList((DocComponent)mock(TextComponent.class)));
        when(docInfo.getComponents()).thenReturn(textComponents);
        when(docInfo.getName()).thenReturn("name");

        return docInfo;
    }

}
