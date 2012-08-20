package org.lawrencebower.docgen.core.generator.overlay.renderer;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.document.component.position.DocAlignment;
import org.lawrencebower.docgen.core.document.component.position.DocCoordinates;
import org.lawrencebower.docgen.core.document.component.position.DocPosition;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.core.generator.utils.ITextTableGeneratorTest;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = {"classpath:META-INF/integration-test-config.xml"})
public class OverlayTableRendererIntegrationTest extends AbstractOverlayRendererTest {

    @Before
    public void setup() {
        super.prepareDirs();
    }

    @Test
    public void testRenderComponent_textAlignment_IsValid() {

        String inputFilePath = inputPackage + "table_renderer_input.pdf";
        String expectedOutputFilePath = inputPackage + "table_renderer_expected_output.pdf";
        String outFilePath = outputPackage + "table_renderer_output.pdf";

        int width = 100;
        int height = 200;

        DocCoordinates leftCoordinates = new DocCoordinates(10, 545, width, height);
        DocPosition leftPosition = new DocPosition(DocAlignment.LEFT, leftCoordinates);
        TableComponent leftTextComponent = getTableComponent(leftPosition);
        leftTextComponent.setRenderBorder(true);

        DocCoordinates rightCoordinates = new DocCoordinates(150, 445, width, height);
        DocPosition rightPosition = new DocPosition(DocAlignment.RIGHT, rightCoordinates);
        TableComponent rightTextComponent = getTableComponent(rightPosition);
        rightTextComponent.setRenderBorder(true);

        DocCoordinates centerCoordinates = new DocCoordinates(300, 345, width, height);
        DocPosition centerPosition = new DocPosition(DocAlignment.CENTER, centerCoordinates);
        TableComponent centerTextComponent = getTableComponent(centerPosition);
        centerTextComponent.setRenderBorder(true);

        createPDFAndCompareWithExpected(expectedOutputFilePath,
                                        outFilePath,
                                        inputFilePath,
                                        leftTextComponent,
                                        rightTextComponent,
                                        centerTextComponent);

    }

    public TableComponent getTableComponent(DocPosition position) {
        TableComponent tableComponent = ITextTableGeneratorTest.makeStandardTableComponent(3, 3);
        tableComponent.setPosition(position);
        return tableComponent;
    }

}
