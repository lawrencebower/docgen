package org.lawrencebower.docgen.core.generator.overlay.renderer;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.document.component.position.DocCoordinates;
import org.lawrencebower.docgen.core.document.component.position.HorizontalAlignment;
import org.lawrencebower.docgen.core.document.component.text.FontInfo;
import org.lawrencebower.docgen.core.document.component.text.FontStyle;
import org.lawrencebower.docgen.core.document.component.text.TextBlock;
import org.lawrencebower.docgen.core.generator.utils.TextGenerator;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "classpath:META-INF/integration-test-config.xml")
public class OverlayTextRendererIntegrationTest extends AbstractOverlayRendererTest {

    /**
     * read from classpath
     */
    String inputFilePath = "/org/lawrencebower/docgen/core/generator/overlay/renderer/text_renderer_input.pdf";

    @Before
    public void setup() {
        prepareDirs();
    }

    @Test
    public void testRenderComponent_textAlignment_IsValid() {

        String expectedOutputFilePath = inputPackage + "text_renderer_expected_output.pdf";
        String outFilePath = outputPackage + "text_renderer_output.pdf";

        int width = 100;
        int height = 200;

        DocCoordinates leftCoordinates = new DocCoordinates(10, 545, width, height);
        TextComponent leftTextComponent = new TextComponent(HorizontalAlignment.LEFT, "left align");
        leftTextComponent.setCoordinates(leftCoordinates);
        leftTextComponent.setRenderBorder(true);

        DocCoordinates rightCoordinates = new DocCoordinates(150, 445, width, height);
        TextComponent rightTextComponent = new TextComponent(HorizontalAlignment.RIGHT, "right align");
        rightTextComponent.setCoordinates(rightCoordinates);
        rightTextComponent.setRenderBorder(true);

        DocCoordinates centerCoordinates = new DocCoordinates(300, 345, width, height);
        TextComponent centerTextComponent = new TextComponent(HorizontalAlignment.CENTER, "center align");
        centerTextComponent.setCoordinates(centerCoordinates);
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

        String expectedOutputFilePath = inputPackage + "text_renderer_expected_output_2.pdf";
        String outFilePath = outputPackage + "text_renderer_output_2.pdf";

        int width = 300;
        int height = 200;

        DocCoordinates leftCoordinates = new DocCoordinates(10, 545, width, height);
        String longText = TextGenerator.multiplyText("text ");
        TextBlock textBlock = new TextBlock(longText, new FontInfo("Serif",20, FontStyle.BOLD_ITALIC));
        TextComponent textComponent = new TextComponent(textBlock);
        textComponent.setAlignment(HorizontalAlignment.LEFT);
        textComponent.setCoordinates(leftCoordinates);
        textComponent.setRenderBorder(true);

        createPDFAndCompareWithExpected(expectedOutputFilePath,
                                        outFilePath,
                                        inputFilePath,
                                        textComponent);

    }

    @Test
    public void testRenderComponent_variedFonts_IsValid() {

        String expectedOutputFilePath = inputPackage + "text_renderer_expected_output_3.pdf";
        String outFilePath = outputPackage + "text_renderer_output_3.pdf";

        int width = 400;
        int height = 200;

        DocCoordinates leftCoordinates = new DocCoordinates(10, 545, width, height);
        TextBlock variedTextBlock = TextGenerator.createVariedTextBlock();
        TextComponent textComponent = new TextComponent(variedTextBlock);
        textComponent.setAlignment(HorizontalAlignment.LEFT);
        textComponent.setCoordinates(leftCoordinates);
        textComponent.setRenderBorder(true);

        createPDFAndCompareWithExpected(expectedOutputFilePath,
                                        outFilePath,
                                        inputFilePath,
                                        textComponent);

    }
}
