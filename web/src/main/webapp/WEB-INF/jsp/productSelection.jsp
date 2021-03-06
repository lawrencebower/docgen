<%@ taglib prefix="sf" uri="http://www.springframework.org/tags/form" %>
<%@ taglib prefix="spring" uri="http://www.springframework.org/tags" %>

<%@ include file="/WEB-INF/jsp/include.jsp" %>

<jsp:useBean id="allProducts"
             scope="request"
             type="java.util.List<org.lawrencebower.docgen.web_model.view.product.ProductView>"/>

<jsp:useBean id="sessionData"
             scope="session"
             type="org.lawrencebower.docgen.web.model.SessionData"/>

<jsp:useBean id="noProductId"
             scope="request"
             type="java.lang.String"/>

<script type="text/javascript">
    function formSubmit() {
        document.getElementById("productSelect").submit();
    }
</script>

<s:url var="customer_url" value="/customerSelect"/>
<a href="${customer_url}">Select customer</a>
&nbsp;->&nbsp;
Select products

<div>

    <br/>

    <sf:form id="productSelect" method="POST"
             action="/docgen/productSelect"
             modelAttribute="productBindBean">

        <sf:select path="productId" onchange="formSubmit()">

            <sf:option value="${noProductId}" label="--- Select ---"/>

            <c:forEach var="product" items="${allProducts}">
                <sf:option value="${product.productId}"
                           label="${product.modelNumber} - ${product.productName}"/>
            </c:forEach>
        </sf:select>
    </sf:form>
    <s:url var="reload_url" value="/reloadProducts"/>
    <a href="${reload_url}">reload products</a>
    <br/>
    <br/>

    <c:if test="${sessionData.hasProducts}">

        <sf:form method="post"
                 name="form"
                 action="/docgen/productSelect/productDetails/"
                 modelAttribute="productSelectionBean">

            <table border="1">
                <tr>
                    <td>Product Name</td>
                    <td>Quantity</td>
                    <td>Cost</td>
                </tr>
                <c:forEach var="selectedProduct" items="${sessionData.selectedProducts}" varStatus="index">
                    <c:set var="count" value="${index.index}"/>

                    <input id="${selectedProduct.productId}"
                           name="products[${count}].productId"
                           value="${selectedProduct.productId}"
                           type="hidden"/>

                    <tr>
                        <td>
                            <c:out value="${selectedProduct.productName}"/>
                            &nbsp;-&nbsp;
                            <c:out value="${selectedProduct.modelNumber}"/>
                        </td>
                        <td align="center">
                            <input id="${selectedProduct.quantity}"
                                   name="products[${count}].quantity"
                                   value="${selectedProduct.quantity}"/>
                        </td>
                        <td align="center">
                            <input id="${selectedProduct.productValue}"
                                   name="products[${count}].value"
                                   value="${selectedProduct.productValue}"/>
                        </td>
                    </tr>
                </c:forEach>
            </table>
            <s:url var="clear_products_url" value="/productSelect/clearProducts"/>
            <a href="${clear_products_url}">
                <c:out value="Clear products"/>
            </a>
            <br/>
            <br/>
            <br/>

            <input value="enter data" type="submit"/>


        </sf:form>

    </c:if>
</div>

