package org.lawrencebower.docgen.core.generator.custom.renderer;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.ImageComponent;
import org.lawrencebower.docgen.core.document.component.position.HorizontalAlignment;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.springframework.core.io.ClassPathResource;
import org.springframework.core.io.Resource;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import static org.junit.Assert.assertEquals;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "classpath:META-INF/integration-test-config.xml")
public class CustomImageRendererIntegrationTest extends AbstractCustomRendererTest {

    private Resource imageFileLocation;

    @Before
    public void setup() {
        prepareDirs();
        imageFileLocation = new ClassPathResource("/org/lawrencebower/docgen/core/generator/overlay/renderer/bod.bmp");
    }

    @Test
    public void testRenderComponent_alignedComponent_validFile() {

        String expectedOutputFilePath = inputPackage + "image_renderer_expected_output.pdf";
        String outFilePath = outputPackage + "image_renderer_output.pdf";

        DocComponent leftImageComponent = new ImageComponent(imageFileLocation);
        leftImageComponent.setAlignment(HorizontalAlignment.LEFT);

        DocComponent centerImageComponent = new ImageComponent(HorizontalAlignment.CENTER, imageFileLocation);

        DocComponent rightImageComponent = new ImageComponent(HorizontalAlignment.RIGHT, imageFileLocation);

        createPDFAndCompareWithExpected(expectedOutputFilePath,
                                        outFilePath,
                                        leftImageComponent,
                                        centerImageComponent,
                                        rightImageComponent);
    }

    @Test
    public void testRenderComponent_scaledComponent_validFile() {

        String expectedOutputFilePath = inputPackage + "image_renderer_expected_output2.pdf";
        String outFilePath = outputPackage + "image_renderer_output2.pdf";

        ImageComponent leftImageComponent = new ImageComponent(HorizontalAlignment.LEFT, imageFileLocation);
        leftImageComponent.setSize(10,10);

        ImageComponent centerImageComponent = new ImageComponent(HorizontalAlignment.CENTER, imageFileLocation);
        centerImageComponent.setSize(50,75);

        ImageComponent rightImageComponent = new ImageComponent(HorizontalAlignment.RIGHT, imageFileLocation);
        rightImageComponent.setSize(100,50);

        createPDFAndCompareWithExpected(expectedOutputFilePath,
                                        outFilePath,
                                        leftImageComponent,
                                        centerImageComponent,
                                        rightImageComponent);
    }

    @Test
    public void testRenderComponent_invalidFilePath_throwsError() {

        String message = "MESSAGE NOT SET";
        String expected = "java.io.FileNotFoundException: class path resource [i dont exist] cannot be opened because it does not exist";

        String expectedOutputFilePath = inputPackage + "image_renderer_expected_output2.pdf";
        String outFilePath = outputPackage + "image_renderer_output2.pdf";

        ImageComponent imageComponent = new ImageComponent(HorizontalAlignment.LEFT, new ClassPathResource("i dont exist"));
        imageComponent.setSize(10, 10);

        try {
            createPDFAndCompareWithExpected(expectedOutputFilePath,
                                            outFilePath,
                                            imageComponent);
        } catch (DocGenException e) {
            message = e.getMessage();
        }

        assertEquals(expected, message);
    }
}
