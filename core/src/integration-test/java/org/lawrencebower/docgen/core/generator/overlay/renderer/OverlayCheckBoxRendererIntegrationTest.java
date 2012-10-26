package org.lawrencebower.docgen.core.generator.overlay.renderer;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.document.component.CheckBoxComponent;
import org.lawrencebower.docgen.core.document.component.position.DocCoordinates;
import org.lawrencebower.docgen.core.document.component.position.HorizontalAlignment;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = {"classpath:META-INF/integration-test-config.xml"})
public class OverlayCheckBoxRendererIntegrationTest extends AbstractOverlayRendererTest {

    @Before
    public void setup() {
        super.prepareDirs();
    }

    @Test
    public void testRenderComponent_componentAlignment_IsValid() {

        String inputFilePath = inputPackage + "checkbox_renderer_input.pdf";
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
}
