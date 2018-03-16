import { Injectable } from '@angular/core';
import { Router } from '@angular/router';
import * as auth0 from 'auth0-js';
import { Subscription } from 'rxjs/Subscription';
import { Observable } from 'rxjs/Observable';
import 'rxjs/add/observable/timer';

import { Location } from '@angular/common';

@Injectable()
export class AuthService {

  userProfile: any;
  refreshSubscription: Subscription;

  requestedScopes = 'openid profile read:territories';

  auth0 = new auth0.WebAuth({
    clientID: 'RUSXtkkcj0yH1218MEfiFc5wTmVpninA',
    domain: 'territories.auth0.com',
    responseType: 'token id_token',
    audience: 'https://jw-maps.com/api',
    redirectUri: location.protocol + '//' + location.hostname + (location.port ? ':' + location.port : '') + '/callback',
    scope: this.requestedScopes
  });

  constructor(public router: Router, public loc: Location) {}

  public login(): void {
    this.auth0.authorize();
  }

  public handleAuthentication(): void {
    this.auth0.parseHash((err, authResult) => {
      if (authResult && authResult.accessToken && authResult.idToken) {
        window.location.hash = '';
        this.setSession(authResult);
      } else if (err) {
        console.error(err);
      }

      const referToUrl = localStorage.getItem('referTo');
      if (referToUrl) {
        localStorage.removeItem('referTo');
        this.router.navigateByUrl(referToUrl);
      }
    });
  }

  public renewToken() {
    this.auth0.checkSession({}, (err, result) => {
      if (err) {
        console.error(err);
      } else {
        this.setSession(result);
      }
    });
  }

  public scheduleRenewal() {
    if (!this.isAuthenticated()) { return; }
    this.unscheduleRenewal();

    const expiresAt = JSON.parse(localStorage.getItem('expires_at'));

    // Use timer to track delay until expiration
    // to run the refresh at the proper time
    const expiresIn$ = Observable.timer(Math.max(1, expiresAt - Date.now()));

    // Once the delay time from above is
    // reached, get a new JWT and schedule
    // additional refreshes
    this.refreshSubscription = expiresIn$.subscribe(
      () => {
        this.renewToken();
        this.scheduleRenewal();
      }
    );
  }

  public unscheduleRenewal() {
    if (this.refreshSubscription) {
      this.refreshSubscription.unsubscribe();
    }
  }

  private setSession(authResult): void {
    const expiresAt = JSON.stringify((authResult.expiresIn * 1000) + new Date().getTime());
    const scopes = authResult.scope || this.requestedScopes || '';

    localStorage.setItem('access_token', authResult.accessToken);
    localStorage.setItem('id_token', authResult.idToken);
    localStorage.setItem('expires_at', expiresAt);
    localStorage.setItem('scopes', JSON.stringify(scopes));

    this.scheduleRenewal();
  }

  public userHasScopes(scopes: Array<string>): boolean {
    const grantedScopes = JSON.parse(localStorage.getItem('scopes')).split(' ');
    return scopes.every(scope => grantedScopes.includes(scope));
  }

  public logout(): void {
    // Remove tokens and expiry time from localStorage
    localStorage.removeItem('access_token');
    localStorage.removeItem('id_token');
    localStorage.removeItem('expires_at');
    this.unscheduleRenewal();
    // Go back to the home route
    this.router.navigate(['/']);
  }

  public isAuthenticated(): boolean {
    // Check whether the current time is past the
    // Access Token's expiry time
    const expiresAt = JSON.parse(localStorage.getItem('expires_at'));
    return new Date().getTime() < expiresAt;
  }

  public getToken(): string {
    return localStorage.getItem('access_token');
  }

  public getProfile(cb): void {
    const accessToken = this.getToken();
    if (!accessToken) {
      throw new Error('Access Token must exist to fetch profile');
    }

    this.auth0.client.userInfo(accessToken, (err, profile) => {
      if (profile) {
        this.userProfile = profile;
      }
      cb(err, profile);
    });
  }

}
