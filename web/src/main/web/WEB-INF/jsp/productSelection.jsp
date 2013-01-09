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

    <c:forEach var="product" items="${products}">

        <s:url var="product_url" value="/productSelect/productId/{productId}">
            <s:param name="productId" value="${product.id}"/>
        </s:url>

        <a href="${product_url}">
            <c:out value="${product.productName}"/>&nbsp;-&nbsp;<c:out value="${product.id}"/>
        </a>
        <br/>
    </c:forEach>
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
                            <c:out value="${selectedProduct.id}"/>
                        </td>
                        <td align="center">
                            <input value="${selectedProduct.quantity}"
                                   id="${selectedProduct.id}"/>
                        </td>
                        <td align="center">
                            <input value="${selectedProduct.productValue}"
                                   id="${selectedProduct.id}"/>
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
