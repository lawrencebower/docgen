package org.lawrencebower.docgen.core.generator.overlay.renderer;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.document.component.CheckBoxComponent;
import org.lawrencebower.docgen.core.document.component.position.DocCoordinates;
import org.lawrencebower.docgen.core.document.component.position.HorizontalAlignment;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.springframework.core.io.ClassPathResource;
import org.springframework.core.io.Resource;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import static junit.framework.TestCase.assertEquals;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "classpath:META-INF/integration-test-config.xml")
public class OverlayCheckBoxRendererIntegrationTest extends AbstractOverlayRendererTest {

    @Before
    public void setup() {
        prepareDirs();
    }

    @Test
    public void testRenderComponent_componentAlignment_IsValid() {

        /**
         * read from classpath
         */
        Resource inputFilePath = new ClassPathResource("/org/lawrencebower/docgen/core/generator/overlay/renderer/checkbox_renderer_input.pdf");

        String expectedOutputFilePath = inputPackage + "checkbox_renderer_expected_output.pdf";
        String outFilePath = outputPackage + "checkbox_renderer_output.pdf";

        int width = 10;
        int height = 10;

        CheckBoxComponent leftCheckBox = new CheckBoxComponent(true, HorizontalAlignment.LEFT);
        DocCoordinates leftCoordinates = new DocCoordinates(10, 545, width, height);
        leftCheckBox.setCoordinates(leftCoordinates);
        leftCheckBox.setRenderBorder(true);

        CheckBoxComponent rightCheckBox = new CheckBoxComponent(HorizontalAlignment.RIGHT);
        DocCoordinates rightCoordinates = new DocCoordinates(150, 445, width, height);
        rightCheckBox.setCoordinates(rightCoordinates);
        rightCheckBox.setSelected(true);
        rightCheckBox.setRenderBorder(true);

        CheckBoxComponent centerCheckBox = new CheckBoxComponent(true);
        centerCheckBox.setAlignment(HorizontalAlignment.CENTER);
        DocCoordinates centerCoordinates = new DocCoordinates(300, 345, width, height);
        centerCheckBox.setCoordinates(centerCoordinates);
        centerCheckBox.setRenderBorder(true);

        createPDFAndCompareWithExpected(expectedOutputFilePath,
                                        outFilePath,
                                        inputFilePath,
                                        leftCheckBox,
                                        rightCheckBox,
                                        centerCheckBox);

    }

    @Test
    public void testRenderComponent_doesntFit_throwsError() {

        String message = "not set";
        String expectedMessage = "Can not fit checkbox into area - Component : 'checkbox'";

        try {
            Resource inputFilePath = new ClassPathResource("/org/lawrencebower/docgen/core/generator/overlay/renderer/checkbox_renderer_input.pdf");

            String expectedOutputFilePath = inputPackage + "checkbox_renderer_expected_output.pdf";
            String outFilePath = outputPackage + "checkbox_renderer_output_2.pdf";

            int width = 1;
            int height = 1;//wont fit

            CheckBoxComponent checkBox = new CheckBoxComponent(true, HorizontalAlignment.LEFT);
            checkBox.setName("checkbox");
            DocCoordinates coordinates = new DocCoordinates(10, 545, width, height);
            checkBox.setCoordinates(coordinates);
            checkBox.setRenderBorder(true);

            createPDFAndCompareWithExpected(expectedOutputFilePath,
                                            outFilePath,
                                            inputFilePath,
                                            checkBox);
        } catch (DocGenException e) {
            message = e.getMessage();
        }

        assertEquals(expectedMessage, message);
    }
}
