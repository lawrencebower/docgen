package org.lawrencebower.docgen.web_model.view.document;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.web_model.view.document.component.DocComponentView;
import org.lawrencebower.docgen.web_model.view.document.component.DocComponentViewImpl;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.util.ArrayList;
import java.util.List;

import static junit.framework.TestCase.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "classpath:META-INF/web-model-beans.xml")
public class DocumentSetImplTest {

    @Autowired
    private DocumentSetFactory documentSetFactory;

    @Test
    public void testGetAllComponentViewsFromDocs_documentsSet_validReturnCount() throws Exception {

        List<DocumentView> documentViews = getDocumentViews(3, 4);//3 docs, 4 components each

        DocumentSet documentSet = documentSetFactory.createDocumentInfoSet(documentViews);

        List<DocComponentView> allComponents = documentSet.getAllComponentViewsFromDocs();

        assertEquals(12, allComponents.size());
    }

    public List<DocumentView> getDocumentViews(int documentNumber, int componentNumber) {

        List<DocumentView> documentViews = new ArrayList<>();

        for (int i = 0; i < documentNumber; i++) {
            DocumentView docView = mock(DocumentView.class);

            addComponentsToView(componentNumber, docView);

            documentViews.add(docView);
        }

        return documentViews;
    }

    private void addComponentsToView(int componentNumber, DocumentView docView) {

        List<DocComponentView> mockComponents = new ArrayList<>();

        for (int i = 0; i < componentNumber; i++) {
            DocComponentView mockComponent = Mockito.mock(DocComponentViewImpl.class);
            mockComponents.add(mockComponent);
        }

        when(docView.getComponentViews()).thenReturn(mockComponents);
    }
}
