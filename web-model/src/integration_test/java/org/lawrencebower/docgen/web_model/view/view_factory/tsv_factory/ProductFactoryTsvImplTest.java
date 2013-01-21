package org.lawrencebower.docgen.web_model.view.view_factory.tsv_factory;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.web_model.view.product.ProductView;
import org.lawrencebower.docgen.web_model.view.view_factory.factory.ProductFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.util.List;

import static junit.framework.TestCase.assertEquals;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "classpath:META-INF/web-model-integration-test-context.xml")
public class ProductFactoryTsvImplTest {

    @Autowired
    private ProductFactory productFactory;

    @Test
    public void testGetCustomers_validFile_correctNumberReturned() throws Exception {
        List<ProductView> products = productFactory.getProductsAsList();
        assertEquals(3, products.size());
    }
}
