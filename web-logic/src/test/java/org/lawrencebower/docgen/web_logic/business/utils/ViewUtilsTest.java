package org.lawrencebower.docgen.web_logic.business.utils;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.document.Document;
import org.lawrencebower.docgen.web_logic.view.document.DocumentView;
import org.lawrencebower.docgen.web_logic.view.document.DocumentViewFactory;
import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentView;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.util.ArrayList;
import java.util.List;

import static junit.framework.TestCase.assertEquals;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "classpath:META-INF/web-logic-test-context.xml")
public class ViewUtilsTest {

    @Autowired
    ViewUtils viewUtils;
    @Autowired
    DocumentViewFactory viewFactory;

    @Test
    public void testGetAllComponentViewsFromDocs_documentsSet_validReturnCount() throws Exception {

        List<DocumentView> documentViews = getDocumentViews(3, 4);//3 docs, 4 components each

        List<DocComponentView> allComponents = viewUtils.getAllComponentViewsFromDocs(documentViews);

        assertEquals(12, allComponents.size());
    }

    public List<DocumentView> getDocumentViews(int documentNumber, int componentNumber) {

        List<DocumentView> documentViews = new ArrayList<>();

        for (int i = 0; i < documentNumber; i++) {
            Document mock = Mockito.mock(Document.class);
            DocumentView docView = viewFactory.createDocumentInfoView(mock);

            addComponentsToView(componentNumber, docView);

            documentViews.add(docView);
        }

        return documentViews;
    }

    private void addComponentsToView(int componentNumber, DocumentView docView) {
        for (int i = 0; i < componentNumber; i++) {
            DocComponentView mockComponent = Mockito.mock(DocComponentView.class);
            docView.addComponentView(mockComponent);
        }
    }
}
