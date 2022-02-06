package com.kuretru.api.common.mapper;

import com.baomidou.mybatisplus.core.injector.AbstractMethod;
import com.baomidou.mybatisplus.core.injector.DefaultSqlInjector;
import com.baomidou.mybatisplus.core.metadata.TableInfo;
import com.kuretru.api.common.mapper.method.GetMaxSequenceMethod;
import com.kuretru.api.common.mapper.method.UpdateSequenceByUuidsMethod;

import java.util.List;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public class MyBatisPlusSqlInjector extends DefaultSqlInjector {

    @Override
    public List<AbstractMethod> getMethodList(Class<?> mapperClass, TableInfo tableInfo) {
        List<AbstractMethod> methodList = super.getMethodList(mapperClass, tableInfo);
        if (BaseSequenceMapper.class.isAssignableFrom(mapperClass)) {
            methodList.add(new GetMaxSequenceMethod("getMaxSequence"));
//            methodList.add(new UpdateSequenceByUuidsMethod("updateSequenceByUuids"));
        }
        return methodList;
    }

}
