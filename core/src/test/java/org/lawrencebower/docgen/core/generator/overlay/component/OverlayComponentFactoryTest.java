package org.lawrencebower.docgen.core.generator.overlay.component;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.document.component.*;
import org.lawrencebower.docgen.core.document.component.position.DocCoordinates;
import org.lawrencebower.docgen.core.document.component.table.layout_table.LayoutTableComponent;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.core.generator.utils.PDFGenUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.Resource;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "classpath:META-INF/core-test-context.xml")
public class OverlayComponentFactoryTest {

    @Autowired
    private OverlayComponentFactory factory;
    PDFGenUtils mockUtils;

    @Before
    public void setup() {
        mockUtils = mock(PDFGenUtils.class);
    }

    @Test
    public void testCreateOverlayComponent_unKnownType_throwsError() {
        try {
            DocComponent unknownComponent = new TableTextComponent("I am unsupported");
            factory.createOverlayComponent(unknownComponent);
        } catch (DocGenException e) {
            String message = e.getMessage();
            boolean condition = message.contains("DocComponent not mapped to OverlayComponent?");
            assertTrue(condition);
        }
    }

    @Test
    public void testCreateOverlayComponent_textComponent_correctComponentReturned() {
        DocComponent textComponent = new TextComponent("Text");
        textComponent.setCoordinates(new DocCoordinates(1, 2, 3, 4));
        OverlayComponent overlayText = factory.createOverlayComponent(textComponent);
        assertTrue(overlayText instanceof OverlayTextComponent);
    }

    @Test
    public void testCreateOverlayComponent_tableComponent_correctComponentReturned() {
        DocComponent tableComponent = new LayoutTableComponent("Table");
        tableComponent.setCoordinates(new DocCoordinates(1, 2, 3, 4));
        OverlayComponent overlayText = factory.createOverlayComponent(tableComponent);
        assertTrue(overlayText instanceof OverlayTableComponent);
    }

    @Test
    public void testCreateOverlayComponent_imageComponent_correctComponentReturned() {
        Resource mockResource = mock(Resource.class);
        DocComponent imageComponent = new ImageComponent(mockResource);
        imageComponent.setCoordinates(new DocCoordinates(1, 2, 3, 4));
        OverlayComponent overlayText = factory.createOverlayComponent(imageComponent);
        assertTrue(overlayText instanceof OverlayImageComponent);
    }

    @Test
    public void testCreateOverlayComponent_checkboxComponent_correctComponentReturned() {
        DocComponent checkBoxComponent = new CheckBoxComponent(true);
        checkBoxComponent.setCoordinates(new DocCoordinates(1, 2, 3, 4));
        OverlayComponent overlayText = factory.createOverlayComponent(checkBoxComponent);
        assertTrue(overlayText instanceof OverlayCheckBoxComponent);
    }

    @Test
    public void testCreateOverlayText_noCoordinates_errorThrown() {
        DocComponent component = new TextComponent("Text");
        testThrowsErrorIfCoordinatesNotSet(component);
    }

    @Test
    public void testCreateOverlayTable_noCoordinates_errorThrown() {
        DocComponent component = new LayoutTableComponent("Table");
        testThrowsErrorIfCoordinatesNotSet(component);
    }

    @Test
    public void testCreateOverlayImage_noCoordinates_errorThrown() {
        Resource mockResource = mock(Resource.class);
        DocComponent component = new ImageComponent(mockResource);
        testThrowsErrorIfCoordinatesNotSet(component);
    }

    @Test
    public void testCreateOverlayCheckbox_noCoordinates_errorThrown() {
        DocComponent component = new CheckBoxComponent(true);
        testThrowsErrorIfCoordinatesNotSet(component);
    }

    private void testThrowsErrorIfCoordinatesNotSet(DocComponent component) {

        String message = "MESSAGE NOT SET";

        try {
            factory.createOverlayComponent(component);
        } catch (DocGenException e) {
            message = e.getMessage();
        }

        boolean condition = message.contains("Coordinates are null for component");
        assertTrue(condition);
    }
}
