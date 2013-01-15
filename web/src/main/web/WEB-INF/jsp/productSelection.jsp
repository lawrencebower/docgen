<%@ taglib prefix="sf" uri="http://www.springframework.org/tags/form" %>
<%@ include file="/WEB-INF/jsp/include.jsp" %>

<jsp:useBean id="products"
             scope="request"
             type="java.util.List<org.lawrencebower.docgen.web_model.view.product.ProductView>"/>

<jsp:useBean id="sessionData"
             scope="session"
             type="org.lawrencebower.docgen.web.model.SessionData"/>

<s:url var="customer_url" value="/customerSelect"/>
<a href="${customer_url}">Select customer</a>
&nbsp;->&nbsp;
Select products

<div>
    <br/>

    <sf:form method="POST" action="/docgen/productSelect" modelAttribute="productSelection">
        <sf:select path="productId" >
            <sf:option value="NONE" label="--- Select ---"/>
            <sf:options items="${products}" itemLabel="productName" itemValue="productId" />
        </sf:select>
        <input name="commit" type="submit"/>
    </sf:form>
    <br/>

    <c:if test="${sessionData.hasProducts}">

        <form method="post"
              name="form"
              action="/docgen/productSelect/productDetails/">

            <table border="1">
                <tr>
                    <td>Product Name</td>
                    <td>Quantity</td>
                    <td>Cost</td>
                </tr>
                <c:forEach var="selectedProduct" items="${sessionData.selectedProducts}">
                    <tr>
                        <td>
                            <c:out value="${selectedProduct.productName}"/>
                            &nbsp;-&nbsp;
                            <c:out value="${selectedProduct.productId}"/>
                        </td>
                        <td align="center">
                            <c:set var="fieldName"
                                   value="${selectedProduct.productId}${selectedProduct.fieldSeparator}${selectedProduct.quantityToken}"/>
                            <input value="${selectedProduct.quantity}"
                                   id="${fieldName}"
                                   name="${fieldName}"/>
                        </td>
                        <td align="center">
                            <c:set var="fieldName"
                                   value="${selectedProduct.productId}${selectedProduct.fieldSeparator}${selectedProduct.valueToken}"/>
                            <input value="${selectedProduct.productValue}"
                                   id="${fieldName}"
                                   name="${fieldName}"/>
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


        </form>

    </c:if>

</div>
