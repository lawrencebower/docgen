package org.lawrencebower.docgen.web.controller.product_selection;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web.controller.data_entry.SessionSetupUtils;
import org.lawrencebower.docgen.web.model.SessionData;
import org.lawrencebower.docgen.web_logic.business.controler_business.product_selection.ProductSelectionCB;
import org.lawrencebower.docgen.web_model.view.product.ProductView;
import org.lawrencebower.docgen.web_model.view.product.binding.ProductBindBean;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.validation.support.BindingAwareModelMap;

import java.util.List;

import static junit.framework.Assert.assertEquals;
import static junit.framework.Assert.assertTrue;
import static org.lawrencebower.docgen.web.test_examples.factory.DocumentFactoryTestImpl.*;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "classpath:META-INF/web-application-test-context.xml")
public class ProductSelectionControllerTest {

    @Autowired
    ProductSelectionCB productSelectionBusiness;
    @Autowired
    private ProductSelectionHelper productHelper;
    @Autowired
    private SessionData sessionData;
    @Autowired
    private SessionSetupUtils sessionUtils;

    private ProductSelectionController controller;

    @Before
    public void setup() {
        controller = new ProductSelectionController();
        controller.setBusiness(productSelectionBusiness);
        controller.setSessionData(sessionData);
        controller.setProductHelper(productHelper);
    }

    @Test
    public void testSelectProduct_validId_allProductsPlacedOnModel() throws Exception {

        String id1 = getProductIdFromProductModel(PRODUCT_MODEL_1);
        String id2 = getProductIdFromProductModel(PRODUCT_MODEL_2);
        String id3 = getProductIdFromProductModel(PRODUCT_MODEL_3);
        String id4 = getProductIdFromProductModel(PRODUCT_MODEL_4);

        BindingAwareModelMap model = new BindingAwareModelMap();
        ProductBindBean productSelectionBean = new ProductBindBean();
        productSelectionBean.setProductId(id1);
        controller.selectProduct(productSelectionBean, model);

        List<ProductView> products = (List<ProductView>) model.get("allProducts");

        assertEquals(4, products.size());
        assertTrue(products.get(0).getProductId().equals(id1));
        assertTrue(products.get(1).getProductId().equals(id2));
        assertTrue(products.get(2).getProductId().equals(id3));
        assertTrue(products.get(3).getProductId().equals(id4));
    }

    @Test
    public void testSelectProduct_validId_correctProductPlacedOnSession() throws Exception {

        String id1 = getProductIdFromProductModel(PRODUCT_MODEL_1);

        BindingAwareModelMap model = new BindingAwareModelMap();
        ProductBindBean productSelectionBean = new ProductBindBean();
        productSelectionBean.setProductId(id1);
        controller.selectProduct(productSelectionBean, model);

        List<ProductView> products = sessionData.getSelectedProducts();

        assertTrue(products.size() == 1);
        assertTrue(products.get(0).getProductId().equals(id1));
    }

    @Test
    public void testSelectProduct_multipleProductsSelected_correctProductPlacedOnSession() throws Exception {

        BindingAwareModelMap model = new BindingAwareModelMap();

        String id1 = getProductIdFromProductModel(PRODUCT_MODEL_1);
        String id2 = getProductIdFromProductModel(PRODUCT_MODEL_2);

        ProductBindBean productBind1 = new ProductBindBean();
        productBind1.setProductId(id1);
        ProductBindBean productBind2 = new ProductBindBean();
        productBind2.setProductId(id2);

        controller.selectProduct(productBind1, model);
        controller.selectProduct(productBind2, model);
        controller.selectProduct(productBind2, model);

        List<ProductView> products = sessionData.getSelectedProducts();

        assertEquals(2, products.size());
        assertEquals(id1, products.get(0).getProductId());
        assertEquals(id2, products.get(1).getProductId());
        assertEquals(2, products.get(1).getQuantity());
    }

    private String getProductIdFromProductModel(String productId) {
        ProductView product = sessionUtils.getProductWithModelNo(productId);
        return product.getProductId();
    }

    @Test
    public void testSelectProduct_unknownProductSelected_errorThrown() throws Exception {

        try {
            BindingAwareModelMap model = new BindingAwareModelMap();
            ProductBindBean productSelection = new ProductBindBean();
            productSelection.setProductId("I dont exist");
            controller.selectProduct(productSelection, model);
        } catch (DocGenException e) {
            String message = e.getMessage();
            assertEquals("product 'I dont exist' not found?!", message);
        }

    }
}
