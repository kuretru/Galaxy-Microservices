package com.kuretru.api.common.configuration;

import com.baomidou.mybatisplus.core.injector.AbstractMethod;
import com.baomidou.mybatisplus.core.injector.DefaultSqlInjector;
import com.baomidou.mybatisplus.core.metadata.TableInfo;
import com.kuretru.api.common.mapper.BaseSequenceMapper;
import com.kuretru.api.common.mapper.method.GetMaxSequenceMethod;

import java.util.List;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public class MyBatisPlusSqlInjector extends DefaultSqlInjector {

    @Override
    public List<AbstractMethod> getMethodList(Class<?> mapperClass, TableInfo tableInfo) {
        List<AbstractMethod> methodList = super.getMethodList(mapperClass, tableInfo);
        if (BaseSequenceMapper.class.isAssignableFrom(mapperClass)) {
            methodList.add(new GetMaxSequenceMethod());
        }
        return methodList;
    }

}
