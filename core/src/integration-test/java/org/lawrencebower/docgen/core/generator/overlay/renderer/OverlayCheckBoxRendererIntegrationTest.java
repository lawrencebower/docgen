package org.lawrencebower.docgen.core.generator.overlay.renderer;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.document.component.CheckBoxComponent;
import org.lawrencebower.docgen.core.document.component.position.DocAlignment;
import org.lawrencebower.docgen.core.document.component.position.DocCoordinates;
import org.lawrencebower.docgen.core.document.component.position.DocPosition;
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

        DocCoordinates leftCoordinates = new DocCoordinates(10, 545, width, height);
        DocPosition leftPosition = new DocPosition(DocAlignment.LEFT, leftCoordinates);
        CheckBoxComponent leftTextComponent = new CheckBoxComponent(true, leftPosition);
        leftTextComponent.setRenderBorder(true);

        DocCoordinates rightCoordinates = new DocCoordinates(150, 445, width, height);
        DocPosition rightPosition = new DocPosition(DocAlignment.RIGHT, rightCoordinates);
        CheckBoxComponent rightTextComponent = new CheckBoxComponent(true, rightPosition);
        rightTextComponent.setRenderBorder(true);

        DocCoordinates centerCoordinates = new DocCoordinates(300, 345, width, height);
        DocPosition centerPosition = new DocPosition(DocAlignment.CENTER, centerCoordinates);
        CheckBoxComponent centerTextComponent = new CheckBoxComponent(true, centerPosition);
        centerTextComponent.setRenderBorder(true);

        createPDFAndCompareWithExpected(expectedOutputFilePath,
                                        outFilePath,
                                        inputFilePath,
                                        leftTextComponent,
                                        rightTextComponent,
                                        centerTextComponent);

    }
}
