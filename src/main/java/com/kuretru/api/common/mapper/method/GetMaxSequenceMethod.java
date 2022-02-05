package com.kuretru.api.common.mapper.method;

import com.baomidou.mybatisplus.core.injector.AbstractMethod;
import com.baomidou.mybatisplus.core.metadata.TableInfo;
import org.apache.ibatis.mapping.MappedStatement;
import org.apache.ibatis.mapping.SqlSource;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public class GetMaxSequenceMethod extends AbstractMethod {

    public GetMaxSequenceMethod(String name) {
        super(name);
    }

    @Override
    public MappedStatement injectMappedStatement(Class<?> mapperClass, Class<?> modelClass, TableInfo tableInfo) {
        String sqlTemplate = "select max(sequence) from ";
        String tableName = tableInfo.getTableName();
        String where = super.sqlWhereEntityWrapper(true, tableInfo);
        String sql = String.format(sqlTemplate, tableName, where);
        SqlSource sqlSource = languageDriver.createSqlSource(configuration, sql, modelClass);
        return this.addSelectMappedStatementForOther(mapperClass, this.methodName, sqlSource, Integer.class);
    }

}
