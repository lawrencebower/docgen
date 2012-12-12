package org.lawrencebower.docgen.web.controller.product_selection;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.doc_examples.ModelFactoryCodeImpl;
import org.lawrencebower.docgen.web.model.SessionData;
import org.lawrencebower.docgen.web_logic.business.controler_business.product_selection.ProductSelectionCB;
import org.lawrencebower.docgen.web_logic.view.product.ProductView;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.validation.support.BindingAwareModelMap;

import java.util.List;

import static junit.framework.Assert.assertEquals;
import static junit.framework.Assert.assertTrue;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "classpath:META-INF/web-application-test-context.xml")
public class ProductSelectionControllerTest {

    @Autowired
    ProductSelectionCB productSelectionBusiness;

    private ProductSelectionController controller;
    private SessionData sessionData;

    @Before
    public void setup() {
        controller = new ProductSelectionController();
        controller.setBusiness(productSelectionBusiness);
        sessionData = new SessionData();
        controller.setSessionData(sessionData);
    }

    @Test
    public void testSelectProduct_validId_allProductsPlacedOnModel() throws Exception {

        String id1 = ModelFactoryCodeImpl.PRODUCT_ID_1;
        String id2 = ModelFactoryCodeImpl.PRODUCT_ID_2;

        BindingAwareModelMap model = new BindingAwareModelMap();
        controller.selectProduct(id1, model);

        List<ProductView> products = (List<ProductView>) model.get("products");

        assertEquals(2, products.size());
        assertTrue(products.get(0).getId().equals(id1));
        assertTrue(products.get(1).getId().equals(id2));
    }

    @Test
    public void testSelectProduct_validId_correctProductPlacedOnSession() throws Exception {

        String id1 = ModelFactoryCodeImpl.PRODUCT_ID_1;

        BindingAwareModelMap model = new BindingAwareModelMap();
        controller.selectProduct(id1, model);

        List<ProductView> products = sessionData.getSelectedProducts();

        assertTrue(products.size() == 1);
        assertTrue(products.get(0).getId().equals(id1));
    }

    @Test
    public void testSelectProduct_multipleProductsSelected_correctProductPlacedOnSession() throws Exception {

        BindingAwareModelMap model = new BindingAwareModelMap();

        String id1 = ModelFactoryCodeImpl.PRODUCT_ID_1;
        String id2 = ModelFactoryCodeImpl.PRODUCT_ID_2;

        controller.selectProduct(id1, model);
        controller.selectProduct(id2, model);
        controller.selectProduct(id2, model);

        List<ProductView> products = sessionData.getSelectedProducts();

        assertTrue(products.size() == 2);
        assertTrue(products.get(0).getId().equals(id1));
        assertTrue(products.get(1).getId().equals(id2));
        assertTrue(products.get(1).getQuantity() == 2);
    }

    @Test
    public void testSelectProduct_unknownProductSelected_errorThrown() throws Exception {

        try {
            BindingAwareModelMap model = new BindingAwareModelMap();
            controller.selectProduct("I dont exist", model);
        } catch (DocGenException e) {
            String message = e.getMessage();
            assertEquals("product 'I dont exist' not found?!", message);
        }

    }
}
