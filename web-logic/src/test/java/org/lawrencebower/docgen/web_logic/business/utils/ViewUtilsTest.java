package org.lawrencebower.docgen.web_logic.business.utils;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.document.DocumentInfo;
import org.lawrencebower.docgen.web_model.view.document_info.DocComponentView;
import org.lawrencebower.docgen.web_model.view.document_info.DocumentInfoView;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.util.ArrayList;
import java.util.List;

import static junit.framework.TestCase.assertEquals;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = {"classpath:META-INF/web-logic-test-context.xml"})
public class ViewUtilsTest {

    @Autowired
    ViewUtils viewUtils;

    @Test
    public void testGetAllComponentViewsFromDocs_documentsSet_validReturnCount() throws Exception {

        List<DocumentInfoView> docInfoViews = getDocInfoViews(3, 4);//3 docs, 4 components each

        List<DocComponentView> allComponents = viewUtils.getAllComponentViewsFromDocs(docInfoViews);

        assertEquals(12, allComponents.size());
    }

    public List<DocumentInfoView> getDocInfoViews(int documentNumber, int componentNumber) {
        List<DocumentInfoView> docInfoViews = new ArrayList<>();

        for (int i = 0; i < documentNumber; i++) {
            DocumentInfo mockInfo = Mockito.mock(DocumentInfo.class);
            DocumentInfoView docView = new DocumentInfoView(mockInfo);

            addComponentsToView(componentNumber, docView);

            docInfoViews.add(docView);
        }

        return docInfoViews;
    }

    private void addComponentsToView(int componentNumber, DocumentInfoView docView) {
        for (int i = 0; i < componentNumber; i++) {
            DocComponentView mockComponent = Mockito.mock(DocComponentView.class);
            docView.addComponentView(mockComponent);
        }
    }
}
