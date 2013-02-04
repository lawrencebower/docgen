package org.lawrencebower.docgen.web.controller.customer_selection;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.web.controller.product_selection.ProductSelectionHelper;
import org.lawrencebower.docgen.web.model.SessionData;
import org.lawrencebower.docgen.web.test_examples.factory.DocumentFactoryTestImpl;
import org.lawrencebower.docgen.web_logic.business.controler_business.customer_selection.CustomerSelectionCB;
import org.lawrencebower.docgen.web_model.view.contact.ContactView;
import org.lawrencebower.docgen.web_model.view.product.Product;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.validation.support.BindingAwareModelMap;

import java.util.List;

import static junit.framework.Assert.assertTrue;
import static junit.framework.TestCase.assertEquals;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "classpath:META-INF/web-application-test-context.xml")
public class CustomerSelectionControllerTest {

    @Autowired
    CustomerSelectionCB business;
    @Autowired
    SessionData sessionData;
    @Autowired
    ProductSelectionHelper productHelper;

    CustomerSelectionController controller;

    @Before
    public void setUp() throws Exception {

        controller = new CustomerSelectionController();

        controller.setBusiness(business);
        controller.setSessionData(sessionData);
        controller.setProductHelper(productHelper);
    }

    @Test
    public void testSelectCustomer_validCustomer_customerSetOnSession() throws Exception {

        BindingAwareModelMap model = new BindingAwareModelMap();

        controller.selectCustomer(DocumentFactoryTestImpl.CUSTOMER_ID_1, model);

        ContactView customer = sessionData.getSelectedCustomer();
        assertEquals(DocumentFactoryTestImpl.CUSTOMER_ID_1, customer.getContactId());
    }

    @Test
    public void testSelectCustomer_validCustomer_businessSetOnSession() throws Exception {

        BindingAwareModelMap model = new BindingAwareModelMap();

        controller.selectCustomer(DocumentFactoryTestImpl.CUSTOMER_ID_2, model);

        ContactView business = sessionData.getSelectedBusiness();
        assertEquals(DocumentFactoryTestImpl.CUSTOMER_ID_1, business.getContactId());
    }

    @Test
    public void testSelectCustomer_validCustomer_productsSetOnModel() throws Exception {

        BindingAwareModelMap model = new BindingAwareModelMap();

        controller.selectCustomer(DocumentFactoryTestImpl.CUSTOMER_ID_1, model);

        List<Product> products = (List<Product>) model.get("allProducts");

        boolean empty = products.isEmpty();
        assertTrue("No products set on page model", !empty);
    }
}
