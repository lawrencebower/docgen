package org.lawrencebower.docgen.web_logic.view.view_factory;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.web_model.view.contact.ContactView;
import org.lawrencebower.docgen.web_model.view.document.DocumentView;
import org.lawrencebower.docgen.web_model.view.document.DocumentViewImpl;
import org.lawrencebower.docgen.web_model.view.product.ProductView;
import org.lawrencebower.docgen.web_model.view.view_factory.CustomerProduct_Document_Mappings;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.util.List;

import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "classpath:META-INF/web-model-test-context.xml")
public class CustomerProduct_Document_MappingsTest {

    CustomerProduct_Document_Mappings mappings;

    private ContactView customer1;
    private ContactView customer2;
    private ProductView product1;
    private ProductView product2;
    private DocumentView doc1;
    private DocumentView doc2;

    @Before
    public void setup() {
        mappings = new CustomerProduct_Document_Mappings();
        addDocumentsToMapper();
    }

    @Test
    public void testGetDocumentsForCustomerAndProduct_validParams_correctDocsReturned_1() {
        List<String> docs = mappings.getDocumentsForCustomerAndProduct(customer1, product1);
        assertEquals(2, docs.size());
        assertTrue(docs.get(0).equals(doc1.getName()));
        assertTrue(docs.get(1).equals(doc2.getName()));
    }

    @Test
    public void testGetDocumentsForCustomerAndProduct_validParams_correctDocsReturned_2() {
        List<String> docs = mappings.getDocumentsForCustomerAndProduct(customer1, product2);
        assertEquals(1, docs.size());
        assertTrue(docs.get(0).equals(doc2.getName()));
    }

    @Test
    public void testGetDocumentsForCustomerAndProduct_validParams_correctDocsReturned_3() {
        List<String> docs = mappings.getDocumentsForCustomerAndProduct(customer2, product1);
        assertEquals(1, docs.size());
        assertTrue(docs.get(0).equals(doc1.getName()));
    }

    @Test
    public void testGetDocumentsForCustomerAndProduct_unmappedProduct_returnsEmpty() {
        List<String> docs = mappings.getDocumentsForCustomerAndProduct(customer2, product2);
        assertTrue(docs.isEmpty());
    }

    private void addDocumentsToMapper() {

        String customer1Name = "customer1";
        String customer2Name = "customer2";

        customer1 = mockCustomer(customer1Name);
        customer2 = mockCustomer(customer2Name);

        String product1Name = "product1";
        String product2Name = "product2";

        product1 = mockProduct(product1Name);
        product2 = mockProduct(product2Name);

        String doc1Name = "doc1";
        String doc2Name = "doc2";

        doc1 = mockDocument(doc1Name);
        doc2 = mockDocument(doc2Name);

        //customer 1
        mappings.addDocument(customer1Name,
                             product1Name,
                             doc1Name);

        mappings.addDocument(customer1Name,
                             product1Name,
                             doc2Name);

        mappings.addDocument(customer1Name,
                             product2Name,
                             doc2Name);

        //customer 2
        mappings.addDocument(customer2Name,
                             product1Name,
                             doc1Name);
    }

    private DocumentView mockDocument(String docName) {
        DocumentView mock = mock(DocumentViewImpl.class);
        when(mock.getName()).thenReturn(docName);
        return mock;
    }

    private ProductView mockProduct(String productId) {
        ProductView mock = mock(ProductView.class);
        when(mock.getProductId()).thenReturn(productId);
        return mock;
    }

    private ContactView mockCustomer(String customerName) {
        ContactView mock = mock(ContactView.class);
        when(mock.getName()).thenReturn(customerName);
        return mock;
    }
}
