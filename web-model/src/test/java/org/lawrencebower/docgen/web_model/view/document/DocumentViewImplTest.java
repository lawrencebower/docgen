package org.lawrencebower.docgen.web_model.view.document;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.document.Document;
import org.lawrencebower.docgen.web_model.view.view_factory.ViewFactory;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.util.Arrays;
import java.util.List;

import static junit.framework.TestCase.assertEquals;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "classpath:META-INF/web-model-beans.xml")
public class DocumentViewImplTest {

    @Autowired
    DocumentViewFactory documentViewFactory;
    private DocumentViewImpl documentView;

    @Before
    public void setUp() {

        documentView = documentViewFactory.createDocumentView();
        Document document = mock(Document.class);
        when(document.getName()).thenReturn("name");
        documentView.setDocument(document);

        ViewFactory mockViewFactory = mock(ViewFactory.class);
        when(mockViewFactory.createDocument(anyString())).thenReturn(mock(DocumentViewImpl.class));
        documentView.setViewFactory(mockViewFactory);
    }

    @Test
    public void testInjectDocuments_allIncluded_correctNumberReturned() throws Exception {
        DocumentInjectionInfo info1 = mockInjectionInfo(true);
        DocumentInjectionInfo info2 = mockInjectionInfo(true);
        List<DocumentView> injectedDocs = documentView.injectDocuments(Arrays.asList(info1, info2));

        assertEquals(2, injectedDocs.size());
    }

    @Test
    public void testInjectDocuments_oneExcluded_correctNumberReturned() throws Exception {
        DocumentInjectionInfo info1 = mockInjectionInfo(true);
        DocumentInjectionInfo info2 = mockInjectionInfo(false);
        List<DocumentView> injectedDocs = documentView.injectDocuments(Arrays.asList(info1, info2));

        assertEquals(1, injectedDocs.size());
    }

    private DocumentInjectionInfo mockInjectionInfo(boolean attributesMatch) {
        DocumentInjectionInfo mock = Mockito.mock(DocumentInjectionInfo.class);
        when(mock.attributesMatch(documentView)).thenReturn(attributesMatch);
        return mock;
    }
}
