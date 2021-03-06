package org.lawrencebower.docgen.web_model.view.document.component;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.when;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "classpath:META-INF/web-model-test-context.xml")
public class DocComponentViewTest {

    @Autowired
    DocComponentViewFactory viewFactory;

    @Test
    public void testDocComponentView_noComponent_throwsError() {
        try {
            TextComponentView view = viewFactory.getTextComponentView();
            view.setComponent(null);
        } catch (DocGenException e) {
            String message = e.getMessage();
            assertEquals(DocComponentViewImpl.NULL_COMPONENT_MESSAGE, message);
        }
    }

    @Test
    public void testGetName_componentHasNoName_returnsDefault() {
        TextComponent mockComponent = Mockito.mock(TextComponent.class);
        when(mockComponent.getName()).thenReturn(null);
        TextComponentView componentView = viewFactory.createTextComponentView(mockComponent);
        String returnedName = componentView.getName();
        Assert.assertEquals(DocComponentViewImpl.NOT_SET_MESSAGE, returnedName);
    }

    @Test
    public void testGetName_componentHasName_returnsName() {

        String name = "phillip";

        TextComponent mockComponent = Mockito.mock(TextComponent.class);
        when(mockComponent.getName()).thenReturn(name);
        TextComponentView componentView = viewFactory.createTextComponentView(mockComponent);
        String returnedName = componentView.getName();

        Assert.assertEquals(name, returnedName);
    }

    @Test
    public void testEquals_componentsHaveSameName_equal() {

        String name1 = "phillip";
        String name2 = "phillip";

        DocComponentViewImpl view1 = makeComponentWithName(name1);
        DocComponentView view2 = makeComponentWithName(name2);

        assertTrue(view1.equals(view2));
    }

    @Test
    public void testEquals_componentsHaveDifferentName_notEqual() {

        String name1 = "phillip";
        String name2 = "chris";

        DocComponentViewImpl view1 = makeComponentWithName(name1);
        DocComponentView view2 = makeComponentWithName(name2);

        assertFalse(view1.equals(view2));
    }

    @Test
    public void testEquals_componentsHaveNullName_notEqual() {

        String name1 = "phillip";
        String name2 = null;

        DocComponentViewImpl view1 = makeComponentWithName(name1);
        DocComponentView view2 = makeComponentWithName(name2);

        assertFalse(view1.equals(view2));
    }

    private DocComponentViewImpl makeComponentWithName(String name) {
        TextComponent mockComponent = Mockito.mock(TextComponent.class);
        when(mockComponent.getName()).thenReturn(name);
        return viewFactory.createTextComponentView(mockComponent);
    }
}