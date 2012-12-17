package org.lawrencebower.docgen.core.generator.model.itext_component.utils;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.document.component.position.DocCoordinates;
import org.lawrencebower.docgen.core.document.component.position.HorizontalAlignment;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.core.generator.model.itext_component.ITextComponent;
import org.lawrencebower.docgen.core.generator.model.itext_component.ITextComponentFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.util.Arrays;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "classpath:META-INF/core-test-context.xml")
public class ITextComponentUtilsTest {

    @Autowired
    ITextComponentUtils iTextComponentUtils;

    @Autowired
    private ITextComponentFactory componentFactory;

    @Test
    public void testCheckCoordinates_validCoordinates_noError() {

        TextComponent component = new TextComponent(HorizontalAlignment.LEFT, "value");
        component.setCoordinates(new DocCoordinates(1, 1, 1, 1));
        ITextComponent overlayText = componentFactory.createTextComponent(component);
        iTextComponentUtils.checkCoordinatesPresent(Arrays.asList(overlayText));
    }

    @Test
    public void testCheckCoordinates_nullCoordinate_throwsError() {

        try {
            TextComponent component = new TextComponent(HorizontalAlignment.LEFT, "value");
            ITextComponent overlayText = componentFactory.createTextComponent(component);
            iTextComponentUtils.checkCoordinatesPresent(Arrays.asList(overlayText));
        } catch (DocGenException e) {
            String message = e.getMessage();
            assertTrue(message.startsWith("Coordinates are null"));
            return;
        }
        fail();//should not get here
    }

}
