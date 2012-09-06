package org.lawrencebower.docgen.core.generator.custom.renderer;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.document.component.position.DocPosition;
import org.lawrencebower.docgen.core.document.component.position.HorizontalAlignment;
import org.lawrencebower.docgen.core.document.component.text.TextBlock;
import org.lawrencebower.docgen.core.generator.utils.TextGenerator;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.util.List;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = {"classpath:META-INF/integration-test-config.xml"})
public class CustomTextRendererIntegrationTest extends AbstractCustomRendererTest {

    @Before
    public void setup() {
        super.prepareDirs();
    }

    @Test
    public void testRenderComponent_validComponent_createsFile() {

        String expectedOutputFilePath = inputPackage + "text_renderer_expected_output.pdf";
        String outFilePath = outputPackage + "text_renderer_output.pdf";

        DocComponent textComponent = new TextComponent("value");

        createPDFAndCompareWithExpected(expectedOutputFilePath,
                                        outFilePath,
                                        textComponent);

    }

    @Test
    public void testRenderComponent_longtext_wrapsSuccessfully() {

        String expectedOutputFilePath = inputPackage + "text_renderer_expected_output2.pdf";
        String outFilePath = outputPackage + "text_renderer_output2.pdf";

        String longText = TextGenerator.multiplyText("long text");
        DocComponent textComponent = new TextComponent(longText);

        createPDFAndCompareWithExpected(expectedOutputFilePath,
                                        outFilePath,
                                        textComponent);
    }

    @Test
    public void testRenderComponent_alignedText_alignedCorrectly() {

        String expectedOutputFilePath = inputPackage + "text_renderer_expected_output3.pdf";
        String outFilePath = outputPackage + "text_renderer_output3.pdf";

        DocPosition justifiedPosition = new DocPosition(HorizontalAlignment.JUSTIFIED);
        String justifiedText = TextGenerator.multiplyText("justified");
        DocComponent justifiedComponent = new TextComponent(justifiedPosition,
                                                            justifiedText);

        DocPosition leftPosition = new DocPosition(HorizontalAlignment.LEFT);
        String leftText = TextGenerator.multiplyText("left");
        DocComponent leftComponent = new TextComponent(leftPosition,
                                                       leftText);

        DocPosition rightPosition = new DocPosition(HorizontalAlignment.RIGHT);
        String rightText = TextGenerator.multiplyText("right");
        DocComponent rightComponent = new TextComponent(rightPosition,
                                                        rightText);

        DocPosition centerPosition = new DocPosition(HorizontalAlignment.CENTER);
        String centerText = TextGenerator.multiplyText("center");
        DocComponent centerComponent = new TextComponent(centerPosition,
                                                         centerText);

        createPDFAndCompareWithExpected(expectedOutputFilePath,
                                        outFilePath,
                                        justifiedComponent,
                                        centerComponent,
                                        leftComponent,
                                        rightComponent);
    }

    @Test
    public void testRenderComponent_variedFonts_rendersSuccessfully() {

        String expectedOutputFilePath = inputPackage + "text_renderer_expected_output4.pdf";
        String outFilePath = outputPackage + "text_renderer_output4.pdf";

        List<TextBlock> textBlocks = TextGenerator.createVariedTextBlocks();

        DocComponent plainComponent = new TextComponent(textBlocks.get(0));

        DocComponent boldComponent = new TextComponent(textBlocks.get(1));

        DocComponent boldItalicComponent = new TextComponent(textBlocks.get(2));

        DocComponent underlineComponent = new TextComponent(textBlocks.get(3));

        createPDFAndCompareWithExpected(expectedOutputFilePath,
                                        outFilePath,
                                        plainComponent,
                                        boldComponent,
                                        boldItalicComponent,
                                        underlineComponent);
    }

    @Test
    public void testRenderComponent_variedFontsInOneBlock_rendersSuccessfully() {

        String expectedOutputFilePath = inputPackage + "text_renderer_expected_output5.pdf";
        String outFilePath = outputPackage + "text_renderer_output5.pdf";

        TextBlock variedTextBlock = TextGenerator.createVariedTextBlock();

        DocComponent textComponent = new TextComponent(variedTextBlock);

        createPDFAndCompareWithExpected(expectedOutputFilePath,
                                        outFilePath,
                                        textComponent);
    }

}
