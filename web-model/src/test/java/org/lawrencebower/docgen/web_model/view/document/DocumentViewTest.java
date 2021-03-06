package org.lawrencebower.docgen.web_model.view.document;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.document.Document;
import org.lawrencebower.docgen.web_model.view.document.component.DocComponentView;
import org.lawrencebower.docgen.web_model.view.document.component.DocComponentViewImpl;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.util.List;

import static junit.framework.TestCase.assertEquals;
import static org.mockito.Mockito.when;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "classpath:META-INF/web-model-test-context.xml")
public class DocumentViewTest {

    @Autowired
    private DocumentViewImpl documentView;

    @Before
    public void setUp(){
        Document mockDoc = mockDocumentsWithComponents();
        documentView.setDocument(mockDoc);
    }

    @Test
    public void testGetComponentsWithName_noComponents_returnsEmptyList() {
        List<DocComponentView> result = documentView.getComponentViewsWithName("I dont exist");
        assertEquals(0, result.size());
    }

    @Test
    public void testGetComponentsWithName_namedComponents_returnsCorrectNumberOfComponents() {
        mockNamedComponents();
        List<DocComponentView> returnedComponent = documentView.getComponentViewsWithName("name2");
        assertEquals(1, returnedComponent.size());
    }

    @Test
    public void testGetComponentsWithName_2namedComponents_returnsCorrectNumberOfComponents() {
        mockNamedComponents();
        documentView.addComponentView(mockComponentView("name2"));//add another one
        List<DocComponentView> returnedComponent = documentView.getComponentViewsWithName("name2");
        assertEquals(2, returnedComponent.size());
    }

    @Test
    public void testGetComponentsWithName_nullNamedComponents_returnsCorrectNumberOfComponents() {
        mockNamedComponents();
        documentView.addComponentView(mockComponentView(null));//make return null
        List<DocComponentView> returnedComponent = documentView.getComponentViewsWithName("name2");
        assertEquals(1, returnedComponent.size());
    }

    @Test
    public void testGetComponentsWithName_noNamedComponents_returnsCorrectNumberOfComponents() {
        List<DocComponentView> returnedComponent = documentView.getComponentViewsWithName("name2");
        assertEquals(0, returnedComponent.size());
    }

    @Test
    public void testGetComponentsWithName_namedComponents_returnsCorrectComponent() {
        mockNamedComponents();
        List<DocComponentView> returnedComponent = documentView.getComponentViewsWithName("name2");
        assertEquals("name2", returnedComponent.get(0).getName());
    }

    private void mockNamedComponents() {
        documentView.addComponentView(mockComponentView("name1"));
        documentView.addComponentView(mockComponentView("name2"));
        documentView.addComponentView(mockComponentView("name3"));
    }

    private DocComponentView mockComponentView(String name) {
        DocComponentView mock = Mockito.mock(DocComponentViewImpl.class);
        when(mock.getName()).thenReturn(name);
        return mock;
    }

    private Document mockDocumentsWithComponents() {
        return Mockito.mock(Document.class);
    }
}
