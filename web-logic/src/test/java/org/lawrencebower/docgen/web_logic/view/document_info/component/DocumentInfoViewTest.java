package org.lawrencebower.docgen.web_logic.view.document_info.component;

import org.junit.Before;
import org.junit.Test;
import org.lawrencebower.docgen.core.document.DocumentInfo;
import org.lawrencebower.docgen.web_logic.view.document_info.DocComponentView;
import org.lawrencebower.docgen.web_logic.view.document_info.DocumentInfoView;
import org.mockito.Mockito;

import java.util.List;

import static junit.framework.TestCase.assertEquals;
import static org.mockito.Mockito.when;

public class DocumentInfoViewTest {

    private DocumentInfoView docInfoView;

    @Before
    public void setUp(){
        DocumentInfo mockDocInfo = mockDocInfoWithComponents();
        docInfoView = new DocumentInfoView(mockDocInfo);
    }

    @Test
    public void testGetComponentsWithName_noComponents_returnsNull() {
        docInfoView.addComponentView(mockComponentView("name1"));
    }

    @Test
    public void testGetComponentsWithName_namedComponents_returnsCorrectNumberOfComponents() {
        mockNamedComponents();
        List<DocComponentView> returnedComponent = docInfoView.getComponentViewsWithName("name2");
        assertEquals(1, returnedComponent.size());
    }

    @Test
    public void testGetComponentsWithName_2namedComponents_returnsCorrectNumberOfComponents() {
        mockNamedComponents();
        docInfoView.addComponentView(mockComponentView("name2"));//add another one
        List<DocComponentView> returnedComponent = docInfoView.getComponentViewsWithName("name2");
        assertEquals(2, returnedComponent.size());
    }

    @Test
    public void testGetComponentsWithName_nullNamedComponents_returnsCorrectNumberOfComponents() {
        mockNamedComponents();
        docInfoView.addComponentView(mockComponentView(null));//make return null
        List<DocComponentView> returnedComponent = docInfoView.getComponentViewsWithName("name2");
        assertEquals(1, returnedComponent.size());
    }

    @Test
    public void testGetComponentsWithName_noNamedComponents_returnsCorrectNumberOfComponents() {
        List<DocComponentView> returnedComponent = docInfoView.getComponentViewsWithName("name2");
        assertEquals(0, returnedComponent.size());
    }

    @Test
    public void testGetComponentsWithName_namedComponents_returnsCorrectComponent() {
        mockNamedComponents();
        List<DocComponentView> returnedComponent = docInfoView.getComponentViewsWithName("name2");
        assertEquals("name2", returnedComponent.get(0).getName());
    }

    private void mockNamedComponents() {
        docInfoView.addComponentView(mockComponentView("name1"));
        docInfoView.addComponentView(mockComponentView("name2"));
        docInfoView.addComponentView(mockComponentView("name3"));
    }

    private DocComponentView mockComponentView(String name) {
        DocComponentView mock = Mockito.mock(DocComponentView.class);
        when(mock.getName()).thenReturn(name);
        return mock;
    }

    private DocumentInfo mockDocInfoWithComponents() {
        return Mockito.mock(DocumentInfo.class);
    }
}
