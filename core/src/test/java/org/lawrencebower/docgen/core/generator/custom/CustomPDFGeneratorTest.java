package org.lawrencebower.docgen.core.generator.custom;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.generator.custom.component.CustomComponent;
import org.lawrencebower.docgen.core.generator.custom.component.CustomTextComponent;
import org.lawrencebower.docgen.core.generator.utils.PDFGenUtilsImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.util.Arrays;
import java.util.List;

import static org.mockito.Mockito.*;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "classpath:META-INF/core-test-context.xml")
public class CustomPDFGeneratorTest {

    @Autowired
    private CustomPDFGenerator pdfGenerator;

    @Test
    public void testGeneratePDF_generatePDF_checksRequiredValuesPresent() throws Exception {

        PDFGenUtilsImpl mockUtils = mock(PDFGenUtilsImpl.class);

        pdfGenerator.setPdfGenUtils(mockUtils);

        CustomDocument document = stubDocument();

        pdfGenerator.generatePDF(document);

        verify(mockUtils).checkRequiredValuesPresent(document);

    }

    public CustomDocument stubDocument() {

        CustomDocument document = mock(CustomDocument.class);

        when(document.getDocType()).thenReturn(null);
        List<CustomComponent> textComponents = (Arrays.asList((CustomComponent)mock(CustomTextComponent.class)));
        when(document.getComponents()).thenReturn(textComponents);
        when(document.getName()).thenReturn("name");

        return document;
    }

}
