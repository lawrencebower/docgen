package org.lawrencebower.docgen.core.generator.overlay.renderer;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.document.component.position.DocAlignment;
import org.lawrencebower.docgen.core.document.component.position.DocCoordinates;
import org.lawrencebower.docgen.core.document.component.position.DocPosition;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = {"classpath:META-INF/integration-test-config.xml"})
public class OverlayTextRendererIntegrationTest extends AbstractOverlayRendererTest {

    @Before
    public void setup() {
        super.prepareDirs();
    }

    @Test
    public void testRenderComponent_textAlignment_IsValid() {

        String inputFilePath = inputPackage + "text_renderer_input.pdf";
        String expectedOutputFilePath = inputPackage + "text_renderer_expected_output.pdf";
        String outFilePath = outputPackage + "text_renderer_output.pdf";

        int width = 100;
        int height = 200;

        DocCoordinates leftCoordinates = new DocCoordinates(10, 545, width, height);
        DocPosition leftPosition = new DocPosition(DocAlignment.LEFT, leftCoordinates);
        DocComponent leftTextComponent = new TextComponent("Name", leftPosition, "left align");

        DocCoordinates rightCoordinates = new DocCoordinates(150, 445, width, height);
        DocPosition rightPosition = new DocPosition(DocAlignment.RIGHT, rightCoordinates);
        DocComponent rightTextComponent = new TextComponent("Name", rightPosition, "right align");

        DocCoordinates centerCoordinates = new DocCoordinates(300, 345, width, height);
        DocPosition centerPosition = new DocPosition(DocAlignment.CENTER, centerCoordinates);
        DocComponent centerTextComponent = new TextComponent("Name", centerPosition, "center align");

        createPDFAndCompareWithExpected(expectedOutputFilePath,
                                        outFilePath,
                                        inputFilePath,
                                        leftTextComponent,
                                        rightTextComponent,
                                        centerTextComponent);

    }
}
