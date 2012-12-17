package org.lawrencebower.docgen.web_logic.business.mapping.customer_product_document;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.web_logic.view.contact.ContactView;
import org.lawrencebower.docgen.web_logic.view.document.DocumentView;
import org.lawrencebower.docgen.web_logic.view.product.ProductView;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.util.List;

import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "classpath:META-INF/web-logic-test-context.xml")
public class CustomerProduct_Document_MappingsTest {

    @Autowired
    CustomerProduct_Document_Mappings mappings;

    private ContactView customer1;
    private ContactView customer2;
    private ProductView product1;
    private ProductView product2;
    private DocumentView doc1;
    private DocumentView doc2;

    @Before
    public void setup() {
        mappings.mappings.clear();
        addDocumentsToMapper();
    }

    @Test
    public void testGetDocumentsForCustomerAndProduct_validParams_correctDocsReturned_1() {
        List<DocumentView> docs = mappings.getDocumentsForCustomerAndProduct(customer1, product1);
        assertEquals(2, docs.size());
        assertTrue(docs.get(0).getName().equals(doc1.getName()));
        assertTrue(docs.get(1).getName().equals(doc2.getName()));
    }

    @Test
    public void testGetDocumentsForCustomerAndProduct_validParams_correctDocsReturned_2() {
        List<DocumentView> docs = mappings.getDocumentsForCustomerAndProduct(customer1, product2);
        assertEquals(1, docs.size());
        assertTrue(docs.get(0).getName().equals(doc2.getName()));
    }

    @Test
    public void testGetDocumentsForCustomerAndProduct_validParams_correctDocsReturned_3() {
        List<DocumentView> docs = mappings.getDocumentsForCustomerAndProduct(customer2, product1);
        assertEquals(1, docs.size());
        assertTrue(docs.get(0).getName().equals(doc1.getName()));
    }

    @Test
    public void testGetDocumentsForCustomerAndProduct_unmappedProduct_returnsEmpty() {
        List<DocumentView> docs = mappings.getDocumentsForCustomerAndProduct(customer2, product2);
        assertTrue(docs.isEmpty());
    }

    private void addDocumentsToMapper() {

        customer1 = mockCustomer("customer1");
        customer2 = mockCustomer("customer2");

        product1 = mockProduct("product1");
        product2 = mockProduct("product2");

        doc1 = mockDocument("doc1");
        doc2 = mockDocument("doc2");

        //customer 1
        mappings.addDocument(customer1,
                             product1,
                             doc1.getName());
        mappings.addDocument(customer1,
                             product1,
                             doc2.getName());
        mappings.addDocument(customer1,
                             product2,
                             doc2.getName());

        //customer 2
        mappings.addDocument(customer2,
                             product1,
                             doc1.getName());
    }

    private DocumentView mockDocument(String docName) {
        DocumentView mock = mock(DocumentView.class);
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
