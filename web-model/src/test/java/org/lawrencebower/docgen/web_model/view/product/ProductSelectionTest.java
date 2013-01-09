package org.lawrencebower.docgen.web_model.view.product;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import static junit.framework.TestCase.assertEquals;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "classpath:META-INF/web-model-beans.xml")
public class ProductSelectionTest {

    @Test
    public void testAddProduct() throws Exception {
        //todo
    }

    @Test
    public void testGetProducts() throws Exception {
        //todo
    }

    @Test
    public void testGetDocumentsForViewing_emptyProducts_throwsError() throws Exception {
        try {
            ProductSelection productSelection = new ProductSelection();
            productSelection.checkProductsSet();
        } catch (DocGenException e) {
            String message = e.getMessage();
            assertEquals(ProductSelection.NO_PRODUCTS_SELECTED, message);
        }
    }
}
