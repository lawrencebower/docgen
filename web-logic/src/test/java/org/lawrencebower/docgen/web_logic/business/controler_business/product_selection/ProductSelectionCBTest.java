package org.lawrencebower.docgen.web_logic.business.controler_business.product_selection;

import org.junit.Before;
import org.junit.Ignore;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.web_model.view.contact.ContactView;
import org.lawrencebower.docgen.web_model.view.document.DocumentSetFactory;
import org.lawrencebower.docgen.web_model.view.document.DocumentView;
import org.lawrencebower.docgen.web_model.view.document.DocumentViewImpl;
import org.lawrencebower.docgen.web_model.view.product.ProductView;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.util.ArrayList;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;


@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "classpath:META-INF/web-logic-test-context.xml")
@Ignore
public class ProductSelectionCBTest {

    @Autowired
    ProductSelectionCB productSelectionBusiness;
    @Autowired
    DocumentSetFactory documentSetFactory;

    @Autowired
    @Qualifier("pdfOutputRoot")
    String fileRoot;

    @Mock
    ContactView mockCustomer;
    @Mock
    ContactView mockBusiness;
    @Mock
    ArgumentCaptor<ArrayList<ProductView>> mockProducts;

    @Before
    public void setup() {
        MockitoAnnotations.initMocks(this);
    }

    //MOCKING

    private DocumentView mockDocumentView(String docName) {
        DocumentView docView = mock(DocumentViewImpl.class);
        when(docView.getName()).thenReturn(docName);
        return docView;
    }

}
