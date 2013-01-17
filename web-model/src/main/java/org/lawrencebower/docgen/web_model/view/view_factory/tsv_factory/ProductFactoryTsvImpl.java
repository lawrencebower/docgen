package org.lawrencebower.docgen.web_model.view.view_factory.tsv_factory;

import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web_model.view.product.Product;
import org.lawrencebower.docgen.web_model.view.product.ProductBuilder;
import org.lawrencebower.docgen.web_model.view.product.ProductView;
import org.lawrencebower.docgen.web_model.view.product.ProductViewFactory;
import org.lawrencebower.docgen.web_model.view.view_factory.factory.ProductFactory;
import org.lawrencebower.docgen.web_model.view.view_factory.tsv_factory.parser.DataRow;
import org.lawrencebower.docgen.web_model.view.view_factory.tsv_factory.parser.DataSet;
import org.lawrencebower.docgen.web_model.view.view_factory.tsv_factory.parser.TSVReader;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class ProductFactoryTsvImpl implements ProductFactory {

    @Autowired
    private TSVReader tsvReader;
    @Autowired
    private ProductViewFactory viewFactory;

    private String productsTSVFile;
    private Map<String, Product> products;

    @Override
    public Map<String, Product> getProducts() {
        return products;
    }

    @Override
    public List<ProductView> getProductsAsList() {

        List<ProductView> results = new ArrayList<>();
        for (Product product : products.values()) {
            ProductView productView = viewFactory.createProductView(product);
            results.add(productView);
        }

        return results;

    }

    @Override
    public boolean hasProduct(String productId) {
        return products.containsKey(productId);
    }

    @Override
    public ProductView getProduct(String productId) {

        if(!hasProduct(productId)){
            String message = String.format("Product it id '%s' not found?!", productId);
            throw new DocGenException(message);
        }

        Product product = products.get(productId);

        return viewFactory.createProductView(product);
    }

    public void setProductsTSVFile(String productsTSVFile) {
        this.productsTSVFile = productsTSVFile;
    }

    public void initProducts() {

        DataSet dataSet = tsvReader.readDataSetAsFile(productsTSVFile);

        products = new LinkedHashMap<>();//use linked map to maintain order for tests

        for (DataRow dataRow : dataSet.getRows()) {
            Product product = mapProductInfo(dataRow);
            String productId = product.getProductId();
            products.put(productId, product);
        }
    }

    private Product mapProductInfo(DataRow dataRow) {

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

        return builder.buildProduct();
    }

}
