<%@ taglib prefix="sf" uri="http://www.springframework.org/tags/form" %>
<%@ include file="/WEB-INF/jsp/include.jsp" %>

hello I am the fields

<jsp:useBean id="sessionData" scope="session" type="org.lawrencebower.docgen.web.model.SessionData"/>

<div>
    <ol>

        <form method="post"
              action="/docgen/dataEntry/setFields">

            <fieldset>

                <c:forEach var="document"
                           items="${sessionData.documents}"
                           varStatus="docIndex">

                    <br/><c:out value="${document.name}"/><br/><br/>

                    <c:forEach var="field"
                               items="${document.editableFields}"
                               varStatus="fieldIndex">

                        <c:if test="${field.isTextComponent}">
                            <label>
                                <c:out value="${field.name}"/>
                                <input name="${document.name}~${field.name}"
                                       value="${field.value}"/>
                            </label>
                            <br/>
                        </c:if>
                    </c:forEach>
                </c:forEach>
                <input name="commit" type="submit"/>
            </fieldset>
        </form>
    </ol>
</div>