package org.lawrencebower.docgen.core.generator.custom.renderer;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.NewLineComponent;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "classpath:META-INF/integration-test-config.xml")
public class CustomNewLineRendererIntegrationTest extends AbstractCustomRendererTest {

    @Before
    public void setup() {
        prepareDirs();
    }

    @Test
    public void testRenderComponent_validComponent_createsFile() {

        String expectedOutputFilePath = inputPackage + "newline_renderer_expected_output.pdf";
        String outFilePath = outputPackage + "newline_renderer_output.pdf";

        DocComponent textComponent1 = new TextComponent("value");
        DocComponent newlineComponent = new NewLineComponent();
        DocComponent textComponent2 = new TextComponent("value2");
        DocComponent textComponent3 = new TextComponent("value3");

        createPDFAndCompareWithExpected(expectedOutputFilePath,
                                        outFilePath,
                                        textComponent1,
                                        newlineComponent,
                                        textComponent2,
                                        textComponent3);
    }

}
