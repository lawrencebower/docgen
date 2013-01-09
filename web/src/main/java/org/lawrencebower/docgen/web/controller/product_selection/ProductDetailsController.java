package org.lawrencebower.docgen.web.controller.product_selection;

import org.apache.log4j.Logger;
import org.lawrencebower.docgen.web.controller.data_entry.PrepareFields;
import org.lawrencebower.docgen.web.model.SessionData;
import org.lawrencebower.docgen.web_logic.business.controler_business.product_selection.ProductSelectionCB;
import org.lawrencebower.docgen.web_model.view.product.ProductView;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.context.request.WebRequest;

import java.util.List;
import java.util.Map;

@Controller
@Scope("session")
public class ProductDetailsController {

    static Logger logger = Logger.getLogger(ProductDetailsController.class);

    private ProductSelectionCB business;
    private SessionData sessionData;
    private PrepareFields prepareFields;

    @Autowired
    public void setBusiness(ProductSelectionCB business) {
        this.business = business;
    }

    @Autowired
    public void setSessionData(SessionData sessionData) {
        this.sessionData = sessionData;
    }

    @Autowired
    public void setPrepareFields(PrepareFields prepareFields) {
        this.prepareFields = prepareFields;
    }

    @RequestMapping(value = "/productSelect/productDetails/", method = RequestMethod.POST)
    public String submitProducts(WebRequest webRequest) {

        Map<String, String[]> parameterMap = webRequest.getParameterMap();

        List<ProductView> products = sessionData.getSelectedProducts();

        business.mapFieldValuesToProducts(parameterMap, products);

        prepareFields.prepareFields();

        return "dataEntry";
    }

}