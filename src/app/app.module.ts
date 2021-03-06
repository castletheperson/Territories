import { BrowserModule } from '@angular/platform-browser';
import { NgModule } from '@angular/core';
import { HttpClientModule, HTTP_INTERCEPTORS } from '@angular/common/http';
import { FormsModule } from '@angular/forms';

import { AngularOpenlayersModule } from 'ngx-openlayers';

import { AppComponent } from './app.component';
import { AppRoutingModule } from './app-routing.module';
import { AuthService } from './auth/auth.service';
import { HomeComponent } from './home/home.component';
import { CallbackComponent } from './callback/callback.component';
import { AuthGuardService } from './auth/auth-guard.service';
import { TerritoryComponent } from './territory/territory.component';
import { TokenInterceptor } from './auth/token.interceptor';
import { TerritoryService } from './territory/territory.service';

@NgModule({
  declarations: [
    AppComponent,
    HomeComponent,
    CallbackComponent,
    TerritoryComponent
  ],
  imports: [
    BrowserModule,
    AppRoutingModule,
    HttpClientModule,
    FormsModule,
    AngularOpenlayersModule
  ],
  providers: [
    TerritoryService,
    AuthService,
    AuthGuardService, {
      provide: HTTP_INTERCEPTORS,
      useClass: TokenInterceptor,
      multi: true
    }],
  bootstrap: [AppComponent]
})
export class AppModule { }
