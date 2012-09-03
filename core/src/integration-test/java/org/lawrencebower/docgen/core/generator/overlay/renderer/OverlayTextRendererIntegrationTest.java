package org.lawrencebower.docgen.core.generator.overlay.renderer;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.document.component.position.DocAlignment;
import org.lawrencebower.docgen.core.document.component.position.DocCoordinates;
import org.lawrencebower.docgen.core.document.component.position.DocPosition;
import org.lawrencebower.docgen.core.document.component.text.FontInfo;
import org.lawrencebower.docgen.core.document.component.text.FontStyle;
import org.lawrencebower.docgen.core.document.component.text.TextBlock;
import org.lawrencebower.docgen.core.generator.utils.TextGenerator;
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
        TextComponent leftTextComponent = new TextComponent(leftPosition, "left align");
        leftTextComponent.setRenderBorder(true);

        DocCoordinates rightCoordinates = new DocCoordinates(150, 445, width, height);
        DocPosition rightPosition = new DocPosition(DocAlignment.RIGHT, rightCoordinates);
        TextComponent rightTextComponent = new TextComponent(rightPosition, "right align");
        rightTextComponent.setRenderBorder(true);

        DocCoordinates centerCoordinates = new DocCoordinates(300, 345, width, height);
        DocPosition centerPosition = new DocPosition(DocAlignment.CENTER, centerCoordinates);
        TextComponent centerTextComponent = new TextComponent(centerPosition, "center align");
        centerTextComponent.setRenderBorder(true);

        createPDFAndCompareWithExpected(expectedOutputFilePath,
                                        outFilePath,
                                        inputFilePath,
                                        leftTextComponent,
                                        rightTextComponent,
                                        centerTextComponent);

    }

    @Test
    public void testRenderComponent_longText_IsValid() {

        String inputFilePath = inputPackage + "text_renderer_input.pdf";
        String expectedOutputFilePath = inputPackage + "text_renderer_expected_output_2.pdf";
        String outFilePath = outputPackage + "text_renderer_output_2.pdf";

        int width = 300;
        int height = 200;

        DocCoordinates leftCoordinates = new DocCoordinates(10, 545, width, height);
        DocPosition leftPosition = new DocPosition(DocAlignment.LEFT, leftCoordinates);
        String longText = TextGenerator.multiplyText("text ");
        TextBlock textBlock = new TextBlock(longText, new FontInfo("Serif",20, FontStyle.BOLD_ITALIC));
        TextComponent textComponent = new TextComponent(textBlock);
        textComponent.setPosition(leftPosition);
        textComponent.setRenderBorder(true);

        createPDFAndCompareWithExpected(expectedOutputFilePath,
                                        outFilePath,
                                        inputFilePath,
                                        textComponent);

    }

    @Test
    public void testRenderComponent_variedFonts_IsValid() {

        String inputFilePath = inputPackage + "text_renderer_input.pdf";
        String expectedOutputFilePath = inputPackage + "text_renderer_expected_output_3.pdf";
        String outFilePath = outputPackage + "text_renderer_output_3.pdf";

        int width = 400;
        int height = 200;

        DocCoordinates leftCoordinates = new DocCoordinates(10, 545, width, height);
        DocPosition leftPosition = new DocPosition(DocAlignment.LEFT, leftCoordinates);
        TextBlock variedTextBlock = TextGenerator.createVariedTextBlock();
        TextComponent textComponent = new TextComponent(variedTextBlock);
        textComponent.setPosition(leftPosition);
        textComponent.setRenderBorder(true);

        createPDFAndCompareWithExpected(expectedOutputFilePath,
                                        outFilePath,
                                        inputFilePath,
                                        textComponent);

    }
}
