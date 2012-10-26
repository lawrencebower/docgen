package org.lawrencebower.docgen.core.generator.custom.renderer;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.LineComponent;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.document.component.position.HorizontalAlignment;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = {"classpath:META-INF/integration-test-config.xml"})
public class CustomLineRendererIntegrationTest extends AbstractCustomRendererTest {

    @Before
    public void setup() {
        super.prepareDirs();
    }

    @Test
    public void testRenderComponent_validComponent_createsFile() {

        String expectedOutputFilePath = inputPackage + "line_renderer_expected_output.pdf";
        String outFilePath = outputPackage + "line_renderer_output.pdf";

        TextComponent textComponent = new TextComponent("text");

        DocComponent lineComponent1 = new LineComponent(10);
        DocComponent lineComponent2 = new LineComponent(HorizontalAlignment.CENTER, 20);
        DocComponent lineComponent3 = new LineComponent(HorizontalAlignment.RIGHT, 50);
        DocComponent lineComponent4 = new LineComponent(100);

        createPDFAndCompareWithExpected(expectedOutputFilePath,
                                        outFilePath,
                                        textComponent,
                                        lineComponent1,
                                        textComponent,
                                        lineComponent2,
                                        textComponent,
                                        lineComponent3,
                                        textComponent,
                                        lineComponent4);
    }

}
