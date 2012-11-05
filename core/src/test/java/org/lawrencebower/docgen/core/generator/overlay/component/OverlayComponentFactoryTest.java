package org.lawrencebower.docgen.core.generator.overlay.component;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.document.component.*;
import org.lawrencebower.docgen.core.document.component.position.DocCoordinates;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.core.generator.utils.PDFGenUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = {"classpath:META-INF/core-test-context.xml"})
public class OverlayComponentFactoryTest {

    @Autowired
    private OverlayComponentFactory factory;
    PDFGenUtils mockUtils;

    @Before
    public void setup(){
        mockUtils = mock(PDFGenUtils.class);
    }

    @Test
    public void testCreateOverlayComponent_unKnownType_throwsError(){
        try {
            DocComponent unknownComponent = new TableTextComponent("I am unsupported");
            factory.createOverlayComponent(unknownComponent);
        } catch (DocGenException e) {
            assertTrue(e.getMessage().contains("DocComponent not mapped to OverlayComponent?"));
        }
    }

    @Test
    public void testCreateOverlayComponent_textComponent_correctComponentReturned(){
        DocComponent textComponent = new TextComponent("Text");
        textComponent.setCoordinates(new DocCoordinates(1,2,3,4));
        OverlayComponent overlayText = factory.createOverlayComponent(textComponent);
        assertTrue(overlayText instanceof OverlayTextComponent);
    }

    @Test
    public void testCreateOverlayComponent_tableComponent_correctComponentReturned(){
        DocComponent tableComponent = new TableComponent("Table");
        tableComponent.setCoordinates(new DocCoordinates(1, 2, 3, 4));
        OverlayComponent overlayText = factory.createOverlayComponent(tableComponent);
        assertTrue(overlayText instanceof OverlayTableComponent);
    }

    @Test
    public void testCreateOverlayComponent_imageComponent_correctComponentReturned(){
        DocComponent imageComponent = new ImageComponent("Image");
        imageComponent.setCoordinates(new DocCoordinates(1, 2, 3, 4));
        OverlayComponent overlayText = factory.createOverlayComponent(imageComponent);
        assertTrue(overlayText instanceof OverlayImageComponent);
    }

    @Test
    public void testCreateOverlayComponent_checkboxComponent_correctComponentReturned(){
        DocComponent checkBoxComponent = new CheckBoxComponent(true);
        checkBoxComponent.setCoordinates(new DocCoordinates(1, 2, 3, 4));
        OverlayComponent overlayText = factory.createOverlayComponent(checkBoxComponent);
        assertTrue(overlayText instanceof OverlayCheckBoxComponent);
    }

}
