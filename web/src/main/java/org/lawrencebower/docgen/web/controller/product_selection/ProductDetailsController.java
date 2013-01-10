package org.lawrencebower.docgen.web.controller.product_selection;

import org.apache.log4j.Logger;
import org.lawrencebower.docgen.web.controller.data_entry.PrepareFieldsController;
import org.lawrencebower.docgen.web.model.SessionData;
import org.lawrencebower.docgen.web_logic.business.controler_business.product_selection.ProductSelectionCB;
import org.lawrencebower.docgen.web_model.view.product.ProductSelection;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.context.request.WebRequest;

import java.util.Map;

@Controller
@Scope("session")
public class ProductDetailsController {

    static Logger logger = Logger.getLogger(ProductDetailsController.class);

    private ProductSelectionCB business;
    private SessionData sessionData;
    private PrepareFieldsController prepareFieldsController;

    @Autowired
    public void setBusiness(ProductSelectionCB business) {
        this.business = business;
    }

    @Autowired
    public void setSessionData(SessionData sessionData) {
        this.sessionData = sessionData;
    }

    @Autowired
    public void setPrepareFieldsController(PrepareFieldsController prepareFieldsController) {
        this.prepareFieldsController = prepareFieldsController;
    }

    @RequestMapping(value = "/productSelect/productDetails/", method = RequestMethod.POST)
    public String submitProducts(WebRequest webRequest) {

        Map<String, String[]> parameterMap = webRequest.getParameterMap();

        ProductSelection productSelection = sessionData.getProductSelection();

        business.mapFieldValuesToProducts(parameterMap, productSelection);

        prepareFieldsController.prepareFields();

        return "dataEntry";
    }

}