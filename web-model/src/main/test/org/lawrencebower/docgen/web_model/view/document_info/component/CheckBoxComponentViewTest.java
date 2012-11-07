package org.lawrencebower.docgen.web_model.view.document_info.component;

import org.junit.Test;
import org.lawrencebower.docgen.core.document.component.CheckBoxComponent;
import org.mockito.Mockito;

import static junit.framework.Assert.assertEquals;
import static org.mockito.Mockito.when;

public class CheckBoxComponentViewTest {

    @Test
    public void testGetComponentValue_notSelected_returnsCorrectValue() throws Exception {
        CheckBoxComponent mockComponent = Mockito.mock(CheckBoxComponent.class);
        when(mockComponent.isSelected()).thenReturn(false);
        CheckBoxComponentView componentView = new CheckBoxComponentView(mockComponent);
        assertEquals(CheckBoxComponentView.UNSELECTED_TEXT, componentView.getComponentValue());
    }

    @Test
    public void testGetComponentValue_selected_returnsCorrectValue() throws Exception {
        CheckBoxComponent mockComponent = Mockito.mock(CheckBoxComponent.class);
        when(mockComponent.isSelected()).thenReturn(true);
        CheckBoxComponentView componentView = new CheckBoxComponentView(mockComponent);
        assertEquals(CheckBoxComponentView.SELECTED_TEXT, componentView.getComponentValue());
    }

}
