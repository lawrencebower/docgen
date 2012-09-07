package org.lawrencebower.docgen.core.generator.overlay.renderer;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.document.component.ImageComponent;
import org.lawrencebower.docgen.core.document.component.position.DocCoordinates;
import org.lawrencebower.docgen.core.document.component.position.DocPosition;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = {"classpath:META-INF/integration-test-config.xml"})
public class OverlayImageRendererIntegrationTest extends AbstractOverlayRendererTest {

    private String imageFileLocation = "L:\\pictures\\random\\bod.bmp";

    @Before
    public void setup() {
        super.prepareDirs();
    }

    @Test
    public void testRenderComponent_imagePositions_IsValid() {

        String inputFilePath = inputPackage + "image_renderer_input.pdf";
        String expectedOutputFilePath = inputPackage + "image_renderer_expected_output.pdf";
        String outFilePath = outputPackage + "image_renderer_output.pdf";

        int width = 100;
        int height = 200;

        DocCoordinates leftCoordinates = new DocCoordinates(10, 545, width, height);
        DocPosition leftPosition = new DocPosition(leftCoordinates);
        ImageComponent imageComponent1 = getImageComponent(leftPosition);
        imageComponent1.setRenderBorder(true);

        DocCoordinates rightCoordinates = new DocCoordinates(150, 445, width + 50, height);
        DocPosition rightPosition = new DocPosition(rightCoordinates);
        ImageComponent imageComponent2 = getImageComponent(rightPosition);
        imageComponent2.setRenderBorder(true);

        DocCoordinates centerCoordinates = new DocCoordinates(350, 345, width, height + 50);
        DocPosition centerPosition = new DocPosition(centerCoordinates);
        ImageComponent imageComponent3 = getImageComponent(centerPosition);
        imageComponent3.setRenderBorder(true);

        createPDFAndCompareWithExpected(expectedOutputFilePath,
                                        outFilePath,
                                        inputFilePath,
                                        imageComponent1,
                                        imageComponent2,
                                        imageComponent3);

    }

    private ImageComponent getImageComponent(DocPosition position) {
        return new ImageComponent(position, imageFileLocation);
    }
}