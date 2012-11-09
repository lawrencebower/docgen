package org.lawrencebower.docgen.web_logic.business.mapping;

import junit.framework.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.web_model.view.contact.Contact;
import org.lawrencebower.docgen.web_model.view.document_info.DocumentInfoView;
import org.lawrencebower.docgen.web_model.view.product.Product;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.util.List;
import java.util.Map;

import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "classpath:META-INF/web-logic-test-context.xml")
public class CustomerProduct_Document_MappingsTest {

    @Autowired
    CustomerProduct_Document_Mappings mappings;

    private Contact customer1;
    private Contact customer2;
    private Product product1;
    private Product product2;
    private DocumentInfoView doc1;
    private DocumentInfoView doc2;

    @Before
    public void setup() {
        mappings.mappings.clear();
        addDocumentsToMapper();
    }

    @Test
    public void testGetDocInfosForCustomerAndProduct_validParams_correctDocsReturned_1() {
        List<DocumentInfoView> docs = mappings.getDocInfosForCustomerAndProduct(customer1, product1);
        assertEquals(2, docs.size());
        assertTrue(docs.get(0).equals(doc1));
        assertTrue(docs.get(1).equals(doc2));
    }

    @Test
    public void testGetDocInfosForCustomerAndProduct_validParams_correctDocsReturned_2() {
        List<DocumentInfoView> docs = mappings.getDocInfosForCustomerAndProduct(customer1, product2);
        assertEquals(1, docs.size());
        assertTrue(docs.get(0).equals(doc2));
    }

    @Test
    public void testGetDocInfosForCustomerAndProduct_validParams_correctDocsReturned_3() {
        List<DocumentInfoView> docs = mappings.getDocInfosForCustomerAndProduct(customer2, product1);
        assertEquals(1, docs.size());
        assertTrue(docs.get(0).equals(doc1));
    }

    @Test
    public void testGetDocInfosForCustomerAndProduct_unmappedProduct_returnsEmpty() {
        List<DocumentInfoView> docs = mappings.getDocInfosForCustomerAndProduct(customer2, product2);
        assertTrue(docs.isEmpty());
    }

    private void addDocumentsToMapper() {

        customer1 = mockCustomer("customer1");
        customer2 = mockCustomer("customer2");

        product1 = mockProduct("product1");
        product2 = mockProduct("product2");

        doc1 = mockDocInfo("doc1");
        doc2 = mockDocInfo("doc2");

        //customer 1
        mappings.addDocument(customer1,
                             product1,
                             doc1);
        mappings.addDocument(customer1,
                             product1,
                             doc2);
        mappings.addDocument(customer1,
                             product2,
                             doc2);

        //customer 2
        mappings.addDocument(customer2,
                             product1,
                             doc1);
    }

    private DocumentInfoView mockDocInfo(String docName) {
        DocumentInfoView mock = mock(DocumentInfoView.class);
        when(mock.getName()).thenReturn(docName);
        return mock;
    }

    private Product mockProduct(String productId) {
        Product mock = mock(Product.class);
        when(mock.getProductId()).thenReturn(productId);
        return mock;
    }

    private Contact mockCustomer(String customerName) {
        Contact mock = mock(Contact.class);
        when(mock.getName()).thenReturn(customerName);
        return mock;
    }
}
