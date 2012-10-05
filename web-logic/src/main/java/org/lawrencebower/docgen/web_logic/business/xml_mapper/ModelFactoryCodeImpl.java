package org.lawrencebower.docgen.web_logic.business.xml_mapper;

import org.lawrencebower.docgen.core.document.DocumentInfo;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web_logic.model.customer.Customer;
import org.lawrencebower.docgen.web_logic.model.mapping.CustomerProduct_Document_Mappings;
import org.lawrencebower.docgen.web_logic.model.product.Product;
import org.lawrencebower.docgen.web_logic.view.customer.CustomerView;
import org.lawrencebower.docgen.web_logic.view.document_info.DocumentInfoView;
import org.lawrencebower.docgen.web_logic.view.product.ProductView;
import org.springframework.beans.factory.annotation.Autowired;
import usecase.CommercialInvoice;
import usecase.DeliveryNote;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;

public class ModelFactoryCodeImpl implements ModelFactory {

    @Autowired
    private CommercialInvoice commercialInvoice;
    @Autowired
    private DeliveryNote deliveryNote;

    private LinkedHashMap<String, CustomerView> customers = new LinkedHashMap<>();

    private LinkedHashMap<String, ProductView> products = new LinkedHashMap<>();

    private LinkedHashMap<String, DocumentInfoView> documents = new LinkedHashMap<>();

    private CustomerProduct_Document_Mappings customerProductDocMappings = new CustomerProduct_Document_Mappings();

    private CustomerView customer1;
    private CustomerView customer2;
    private ProductView product1;
    private ProductView product2;
    private DocumentInfoView commercialInvoiceView;
    private DocumentInfoView deliveryNoteView;

    public void init() {
        initCustomers();
        initProducts();
        initDocuments();
        initCustomerProductDocumentMappings();
    }

    private void initDocuments() {

        DocumentInfo commercialInvoiceDocInfo = commercialInvoice.getDocInfo();

        commercialInvoiceView = new DocumentInfoView(commercialInvoiceDocInfo);

        DocumentInfo deliveryNoteDocInfo = deliveryNote.getDocInfo();

        deliveryNoteView = new DocumentInfoView(deliveryNoteDocInfo);

        documents.put(commercialInvoiceView.getName(), commercialInvoiceView);
        documents.put(deliveryNoteView.getName(), deliveryNoteView);
    }

    private void initProducts() {

        product1 = new ProductView(new Product("id1", "name 1"));
        product2 = new ProductView(new Product("id2", "name 2"));

        products.put(product1.getId(), product1);
        products.put(product2.getId(), product2);
    }

    private void initCustomers() {

        customer1 = new CustomerView(new Customer("Customer 1",
                                                  "Just round the corner"));

        customer2 = new CustomerView(new Customer("Customer 2",
                                                  "miles away"));

        customers.put(customer1.getCustomerName(), customer1);
        customers.put(customer2.getCustomerName(), customer2);
    }

    private void initCustomerProductDocumentMappings() {

        //Customer 1
        customerProductDocMappings.addDocument(customer1.getCustomer(),
                                               product1.getproduct(),
                                               commercialInvoiceView.getDocument());

        customerProductDocMappings.addDocument(customer1.getCustomer(),
                                               product2.getproduct(),
                                               commercialInvoiceView.getDocument());

        customerProductDocMappings.addDocument(customer1.getCustomer(),
                                               product2.getproduct(),
                                               deliveryNoteView.getDocument());

        //Customer 2
        customerProductDocMappings.addDocument(customer2.getCustomer(),
                                               product1.getproduct(),
                                               commercialInvoiceView.getDocument());

        customerProductDocMappings.addDocument(customer2.getCustomer(),
                                               product2.getproduct(),
                                               commercialInvoiceView.getDocument());

        customerProductDocMappings.addDocument(customer2.getCustomer(),
                                               product2.getproduct(),
                                               deliveryNoteView.getDocument());

    }

    @Override
    public List<CustomerView> getCustomers() {
        return new ArrayList<>(customers.values());
    }

    @Override
    public List<ProductView> getProducts() {
        return new ArrayList<>(products.values());
    }

    public List<DocumentInfoView> getDocuments() {
        return new ArrayList<>(documents.values());
    }

    public CustomerView getCustomer(String customerName) {
        if (!customers.containsKey(customerName)) {
            throw new DocGenException(String.format("Customer %s not found?!", customerName));
        }
        return customers.get(customerName);
    }

    @Override
    public ProductView getProduct(String productId) {
        if (!products.containsKey(productId)) {
            throw new DocGenException(String.format("product %s not found?!", productId));
        }
        return products.get(productId);
    }
}
