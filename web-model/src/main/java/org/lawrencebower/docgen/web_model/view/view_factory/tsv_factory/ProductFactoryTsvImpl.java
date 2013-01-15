package org.lawrencebower.docgen.web_model.view.view_factory.tsv_factory;

import org.lawrencebower.docgen.web_model.view.product.Product;
import org.lawrencebower.docgen.web_model.view.product.ProductBuilder;
import org.lawrencebower.docgen.web_model.view.product.ProductView;
import org.lawrencebower.docgen.web_model.view.product.ProductViewFactory;
import org.lawrencebower.docgen.web_model.view.view_factory.factory.ProductFactory;
import org.lawrencebower.docgen.web_model.view.view_factory.tsv_factory.parser.DataRow;
import org.lawrencebower.docgen.web_model.view.view_factory.tsv_factory.parser.DataSet;
import org.lawrencebower.docgen.web_model.view.view_factory.tsv_factory.parser.TSVReader;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.HashMap;
import java.util.Map;

public class ProductFactoryTsvImpl implements ProductFactory {

    @Autowired
    private TSVReader tsvReader;

    private String productsTSVFile;

    @Autowired
    private ProductViewFactory viewFactory;

    private Map<String, ProductView> products;

    @Override
    public Map<String, ProductView> getProducts() {
        return products;
    }

    public void setProductsTSVFile(String productsTSVFile) {
        this.productsTSVFile = productsTSVFile;
    }

    public void initProducts() {

        DataSet dataSet = tsvReader.readDataSetAsFile(productsTSVFile);

        products = new HashMap<>();

        for (DataRow dataRow : dataSet.getRows()) {
            ProductView product = mapProductInfo(dataRow);
            String productId = product.getProductId();
            products.put(productId, product);
        }
    }

    private ProductView mapProductInfo(DataRow dataRow) {

        ProductBuilder builder = new ProductBuilder();

        String id = dataRow.getString(0);
        builder.setProductId(id);

        String name = dataRow.getString(1);
        builder.setProductName(name);

        String value = dataRow.getString(2);
        builder.setValue(value);

        String country = dataRow.getString(3);
        builder.setCountryOfOrigin(country);

        String tariffNumber = dataRow.getString(4);
        builder.setHarmonizedTariffNumber(tariffNumber);

        String customsDesc = dataRow.getString(5);
        builder.setCustomsDescription(customsDesc);

        if (dataRow.hasColumn(6)) {
            String[] attributes = dataRow.getStringArray(6);
            builder.setAttributes(attributes);
        }

        Product product = builder.buildProduct();

        return viewFactory.createProductView(product);
    }

}
