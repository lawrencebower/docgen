package org.lawrencebower.docgen.core.generator.custom.renderer;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.document.component.position.DocAlignment;
import org.lawrencebower.docgen.core.document.component.position.DocPosition;
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

        DocComponent textComponent = new TextComponent("Name", "value");

        createPDFAndCompareWithExpected(expectedOutputFilePath,
                                        outFilePath,
                                        textComponent);

    }

    @Test
    public void testRenderComponent_longtext_wrapsSuccessfully() {

        String expectedOutputFilePath = inputPackage + "text_renderer_expected_output2.pdf";
        String outFilePath = outputPackage + "text_renderer_output2.pdf";

        String longText = TextGenerator.multiplyText("long text");
        DocComponent textComponent = new TextComponent("Name", longText);

        createPDFAndCompareWithExpected(expectedOutputFilePath,
                                        outFilePath,
                                        textComponent);
    }

    @Test
    public void testRenderComponent_alignedText_alignedCorrectly() {

        String expectedOutputFilePath = inputPackage + "text_renderer_expected_output3.pdf";
        String outFilePath = outputPackage + "text_renderer_output3.pdf";

        DocPosition justifiedPosition = new DocPosition(DocAlignment.JUSTIFIED);
        String justifiedText = TextGenerator.multiplyText("justified");
        DocComponent justifiedComponent = new TextComponent("Name",
                                                            justifiedPosition,
                                                            justifiedText);

        DocPosition leftPosition = new DocPosition(DocAlignment.LEFT);
        String leftText = TextGenerator.multiplyText("left");
        DocComponent leftComponent = new TextComponent("Name",
                                                       leftPosition,
                                                       leftText);

        DocPosition rightPosition = new DocPosition(DocAlignment.RIGHT);
        String rightText = TextGenerator.multiplyText("right");
        DocComponent rightComponent = new TextComponent("Name",
                                                        rightPosition,
                                                        rightText);

        DocPosition centerPosition = new DocPosition(DocAlignment.CENTER);
        String centerText = TextGenerator.multiplyText("center");
        DocComponent centerComponent = new TextComponent("Name",
                                                         centerPosition,
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

        DocComponent plainComponent = new TextComponent("Plain", textBlocks.get(0));

        DocComponent boldComponent = new TextComponent("bold", textBlocks.get(1));

        DocComponent boldItalicComponent = new TextComponent("bold italic", textBlocks.get(2));

        DocComponent underlineComponent = new TextComponent("underline", textBlocks.get(3));

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

        List<TextBlock> textBlocks = TextGenerator.createVariedTextBlocks();

        TextBlock variedTextBlock = TextGenerator.createVariedTextBlock();

        DocComponent textComponent = new TextComponent("multi text", variedTextBlock);

        createPDFAndCompareWithExpected(expectedOutputFilePath,
                                        outFilePath,
                                        textComponent);
    }

}
