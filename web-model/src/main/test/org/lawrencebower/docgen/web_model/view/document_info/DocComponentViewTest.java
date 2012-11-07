package org.lawrencebower.docgen.web_model.view.document_info;

import junit.framework.Assert;
import org.junit.Test;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web_model.view.document_info.component.TextComponentView;
import org.mockito.Mockito;

import javax.swing.text.ComponentView;

import static junit.framework.Assert.assertTrue;
import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.assertFalse;
import static org.mockito.Mockito.when;

public class DocComponentViewTest {

    @Test
    public void testDocComponentView_noComponent_throwsError() {
        try {
            new TextComponentView(null);
        } catch (DocGenException e) {
            String message = e.getMessage();
            assertEquals(DocComponentView.NULL_COMPONENT_MESSAGE, message);
        }
    }

    @Test
    public void testGetName_componentHasNoName_returnsDefault() {
        TextComponent mockComponent = Mockito.mock(TextComponent.class);
        when(mockComponent.getName()).thenReturn(null);
        TextComponentView componentView = new TextComponentView(mockComponent);
        String returnedName = componentView.getName();
        Assert.assertEquals(DocComponentView.NOT_SET_MESSAGE, returnedName);
    }

    @Test
    public void testGetName_componentHasName_returnsName() {

        String name = "phillip";

        TextComponent mockComponent = Mockito.mock(TextComponent.class);
        when(mockComponent.getName()).thenReturn(name);
        TextComponentView componentView = new TextComponentView(mockComponent);
        String returnedName = componentView.getName();

        Assert.assertEquals(name, returnedName);
    }

    @Test
    public void testEquals_componentsHaveSameName_equal() {

        String name1 = "phillip";
        String name2 = "phillip";

        DocComponentView view1 = makeComponentWithName(name1);
        DocComponentView view2 = makeComponentWithName(name2);

        assertTrue(view1.equals(view2));
    }

    @Test
    public void testEquals_componentsHaveDifferentName_notEqual() {

        String name1 = "phillip";
        String name2 = "chris";

        DocComponentView view1 = makeComponentWithName(name1);
        DocComponentView view2 = makeComponentWithName(name2);

        assertFalse(view1.equals(view2));
    }

    @Test
    public void testEquals_componentsHaveNullName_notEqual() {

        String name1 = "phillip";
        String name2 = null;

        DocComponentView view1 = makeComponentWithName(name1);
        DocComponentView view2 = makeComponentWithName(name2);

        assertFalse(view1.equals(view2));
    }

    private DocComponentView makeComponentWithName(String name) {
        TextComponent mockComponent = Mockito.mock(TextComponent.class);
        when(mockComponent.getName()).thenReturn(name);
        return new TextComponentView(mockComponent);
    }
}