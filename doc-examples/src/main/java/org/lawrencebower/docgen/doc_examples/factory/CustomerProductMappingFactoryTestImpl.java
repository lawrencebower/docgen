package org.lawrencebower.docgen.doc_examples.factory;

import org.lawrencebower.docgen.doc_examples.commercial_invoice.CommercialInvoice;
import org.lawrencebower.docgen.doc_examples.delivery_note.DeliveryNote;
import org.lawrencebower.docgen.doc_examples.fcc_740.FCC_740;
import org.lawrencebower.docgen.doc_examples.fda_2887.FDA_2887;
import org.lawrencebower.docgen.web_model.view.contact.Contact;
import org.lawrencebower.docgen.web_model.view.contact.ContactBuilder;
import org.lawrencebower.docgen.web_model.view.contact.ContactView;
import org.lawrencebower.docgen.web_model.view.contact.ContactViewFactory;
import org.lawrencebower.docgen.web_model.view.document.DocumentView;
import org.lawrencebower.docgen.web_model.view.product.Product;
import org.lawrencebower.docgen.web_model.view.product.ProductBuilder;
import org.lawrencebower.docgen.web_model.view.product.ProductView;
import org.lawrencebower.docgen.web_model.view.product.ProductViewFactory;
import org.lawrencebower.docgen.web_model.view.view_factory.CustomerProduct_Document_Mappings;
import org.lawrencebower.docgen.web_model.view.view_factory.factory.CustomerProductMappingFactory;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.List;

import static org.lawrencebower.docgen.doc_examples.factory.CustomerFactoryTestImpl.CUSTOMER_ID_1;
import static org.lawrencebower.docgen.doc_examples.factory.CustomerFactoryTestImpl.CUSTOMER_ID_2;
import static org.lawrencebower.docgen.doc_examples.factory.ProductFactoryTestImpl.PRODUCT_ID_1;
import static org.lawrencebower.docgen.doc_examples.factory.ProductFactoryTestImpl.PRODUCT_ID_2;

class CustomerProductMappingFactoryTestImpl implements CustomerProductMappingFactory {

    private CustomerProduct_Document_Mappings customerProductDocMappings = new CustomerProduct_Document_Mappings();

    @Autowired
    private ContactViewFactory contactViewFactory;
    @Autowired
    private ProductViewFactory productViewFactory;

    private ProductView product1;
    private ProductView product2;

    @Override
    public CustomerProduct_Document_Mappings getMappingInfo(List<ContactView> customers,
                                                            List<ProductView> products,
                                                            List<DocumentView> documents) {
        return customerProductDocMappings;
    }

    private void initMappings() {

        product1 = buildProduct(PRODUCT_ID_1);
        product2 = buildProduct(PRODUCT_ID_2);

        initCustomer1();
        initCustomer2();
    }

    private void initCustomer2() {

        ContactView customerView2 = buildContact(CUSTOMER_ID_2);

        addMappingInfo(customerView2,
                       product1,
                       CommercialInvoice.INVOICE_NAME);

        addMappingInfo(customerView2,
                       product1,
                       DeliveryNote.DELIVERY_NOTE_NAME);

        addMappingInfo(customerView2,
                       product2,
                       CommercialInvoice.INVOICE_NAME);

        addMappingInfo(customerView2,
                       product1,
                       DeliveryNote.DELIVERY_NOTE_NAME);

        addMappingInfo(customerView2,
                       product1,
                       FDA_2887.FDA_2887_NAME);

        addMappingInfo(customerView2,
                       product1,
                       FCC_740.FCC_740_NAME);
    }

    private void initCustomer1() {

        ContactView customerView1 = buildContact(CUSTOMER_ID_1);

        addMappingInfo(customerView1,
                       product1,
                       CommercialInvoice.INVOICE_NAME);

        addMappingInfo(customerView1,
                       product1,
                       DeliveryNote.DELIVERY_NOTE_NAME);

        addMappingInfo(customerView1,
                       product2,
                       CommercialInvoice.INVOICE_NAME);

        addMappingInfo(customerView1,
                       product2,
                       DeliveryNote.DELIVERY_NOTE_NAME);
    }

    private ProductView buildProduct(String id) {
        ProductBuilder builder = new ProductBuilder();
        builder.setProductId(id);
        Product product = builder.buildProduct();
        return productViewFactory.createProductView(product);
    }

    private ContactView buildContact(String id) {
        ContactBuilder contactBuilder = new ContactBuilder();
        Contact customer2 = contactBuilder.buildContact();
        return contactViewFactory.createContactView(customer2, id);
    }

    private void addMappingInfo(ContactView customer,
                                ProductView product,
                                String documentName) {

        customerProductDocMappings.addDocument(customer, product, documentName);
    }
}
